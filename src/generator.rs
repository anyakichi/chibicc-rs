use std::io::Write;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::parser::{Declaration, Definition, Node, Statement, Type};

static ARGREGS8: &[&str] = &["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
static ARGREGS64: &[&str] = &["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

struct SectionWriter<'a> {
    data: &'a mut dyn Write,
    text: &'a mut dyn Write,
}

#[derive(Debug)]
struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    funcname: &'a str,
    variables: Vec<(Declaration, i64)>,
}

impl<'a> Context<'a> {
    fn new(parent: Option<&'a Context>, funcname: &'a str) -> Self {
        Self {
            parent,
            funcname,
            variables: Vec::new(),
        }
    }

    fn add_variable(&mut self, decl: &Declaration) -> i64 {
        let offset = if self.parent.is_some() {
            match self.variables.last() {
                Some((decl, off)) => off + decl.typ.size() as i64,
                None => 0,
            }
        } else {
            0
        };
        self.variables.push((decl.clone(), offset));
        offset
    }

    fn get_variable(&self, name: &str) -> Option<&(Declaration, i64)> {
        self.variables
            .iter()
            .rev()
            .find(|(decl, _)| decl.name == name)
            .or_else(|| self.parent.and_then(|x| x.get_variable(name)))
    }

    fn get_variable_declaration(&self, name: &str) -> Option<&Declaration> {
        self.get_variable(name).map(|(decl, _)| decl)
    }

    fn get_variable_offset(&self, name: &str) -> Option<i64> {
        self.get_variable(name).map(|(_, offset)| *offset)
    }
}

fn count() -> usize {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    COUNT.fetch_add(1, Ordering::SeqCst)
}

fn align_to(n: i64, align: i64) -> i64 {
    (n + align - 1) / align * align
}

fn push(w: &mut SectionWriter) -> std::io::Result<()> {
    writeln!(w.text, "  push %rax")
}

fn pop(w: &mut SectionWriter, r: &str) -> std::io::Result<()> {
    writeln!(w.text, "  pop {}", r)
}

fn load(w: &mut SectionWriter, typ: &Type) -> std::io::Result<()> {
    match typ {
        Type::Array(_, _) => Ok(()),
        _ => {
            if typ.size() == 1 {
                writeln!(w.text, "  movsbq (%rax), %rax")
            } else {
                writeln!(w.text, "  mov (%rax), %rax")
            }
        }
    }
}

fn store(w: &mut SectionWriter, typ: &Type) -> std::io::Result<()> {
    pop(w, "%rdi")?;
    if typ.size() == 1 {
        writeln!(w.text, "  mov %rl, (%rdi)")
    } else {
        writeln!(w.text, "  mov %rax, (%rdi)")
    }
}

fn find_variable(vars: &mut Vec<(Declaration, i64)>, stmt: &Statement) {
    match stmt {
        Statement::Block(block) => {
            for stmt in block {
                find_variable(vars, stmt)
            }
        }
        Statement::Decl(decls) => {
            let mut offset = match vars.last() {
                Some((decl, off)) => off + decl.typ.size() as i64,
                None => 0,
            };
            for decl in decls {
                vars.push((decl.clone(), offset));
                offset += decl.typ.size() as i64;
            }
        }
        _ => (),
    }
}

fn generate_expr(w: &mut SectionWriter, node: &Node, ctx: &Context) -> std::io::Result<Type> {
    fn gen(
        w: &mut SectionWriter,
        rhs: &Node,
        lhs: &Node,
        ctx: &Context,
    ) -> std::io::Result<(Type, Type)> {
        let rt = generate_expr(w, rhs, ctx)?;
        push(w)?;
        let lt = generate_expr(w, lhs, ctx)?;
        pop(w, "%rdi")?;
        Ok((rt, lt))
    }

    fn gen_addr(w: &mut SectionWriter, name: &str, ctx: &Context) -> std::io::Result<()> {
        let offset = ctx.get_variable_offset(name).unwrap();
        if offset != 0 {
            writeln!(w.text, "  lea {}(%rbp), %rax", offset)
        } else {
            writeln!(w.text, "  lea {}(%rip), %rax", name)
        }
    }

    fn cmp(
        w: &mut SectionWriter,
        rhs: &Node,
        lhs: &Node,
        op: &str,
        ctx: &Context,
    ) -> std::io::Result<()> {
        gen(w, rhs, lhs, ctx)?;
        writeln!(w.text, "  cmp %rdi, %rax")?;
        writeln!(w.text, "  {} %al", op)?;
        writeln!(w.text, "  movzb %al, %rax")
    }

    Ok(match node {
        Node::Assign(lhs, rhs) => {
            match &**lhs {
                Node::Var(name) => {
                    gen_addr(w, name, ctx)?;
                }
                Node::Deref(lhs) => {
                    generate_expr(w, &*lhs, ctx)?;
                }
                _ => {
                    panic!("unexpedted lhs of assign")
                }
            }
            push(w)?;
            let t = generate_expr(w, &*rhs, ctx)?;
            store(w, &t)?;
            t
        }
        Node::Call(name, args) => {
            for arg in args.iter() {
                generate_expr(w, arg, ctx)?;
                push(w)?;
            }

            for reg in ARGREGS64[0..args.len()].iter().rev() {
                pop(w, reg)?;
            }

            writeln!(w.text, "  mov $0, %rax")?;
            writeln!(w.text, "  call {}", name)?;
            Type::Int
        }
        Node::Var(name) => {
            let t = &ctx.get_variable_declaration(name).unwrap().typ;
            gen_addr(w, name, ctx)?;
            load(w, t)?;
            t.clone()
        }
        Node::Str(_, s) => {
            static COUNT: AtomicUsize = AtomicUsize::new(0);
            let i = COUNT.fetch_add(1, Ordering::SeqCst);
            let name = format!(".L..{}", i);
            let t = Type::Array(Box::new(Type::Char), s.len() + 1);
            writeln!(w.data, "  .data")?;
            writeln!(w.data, "  .globl {}", name)?;
            writeln!(w.data, "{}:", name)?;
            for c in s.bytes() {
                writeln!(w.data, "  .byte {}", c)?;
            }
            writeln!(w.data, "  .byte 0")?;
            writeln!(w.text, "  lea {}(%rip), %rax", name)?;
            t
        }
        Node::Integer(i) => {
            writeln!(w.text, "  mov ${}, %rax", i)?;
            Type::Int
        }
        Node::Neg(lhs) => {
            generate_expr(w, &*lhs, ctx)?;
            writeln!(w.text, "  neg %rax")?;
            Type::Int
        }
        Node::Deref(lhs) => match generate_expr(w, &*lhs, ctx)? {
            Type::Pointer(t) | Type::Array(t, _) => {
                load(w, &*t)?;
                *t
            }
            _ => panic!("invalid type for deref"),
        },
        Node::Addr(lhs) => match &**lhs {
            Node::Var(name) => {
                gen_addr(w, name, ctx)?;
                Type::Pointer(Box::new(
                    ctx.get_variable_declaration(name).unwrap().typ.clone(),
                ))
            }
            _ => panic!("unexpected lhs of addr"),
        },
        Node::Add(lhs, rhs) => {
            let t = match gen(w, &*rhs, &*lhs, ctx)? {
                (Type::Char, Type::Char) => Type::Char,
                (Type::Int, Type::Int) | (Type::Int, Type::Char) | (Type::Char, Type::Int) => {
                    Type::Int
                }
                (Type::Int, t @ Type::Pointer(_)) => {
                    writeln!(w.text, "  imul ${}, %rdi", t.size())?;
                    t
                }
                (Type::Int, Type::Array(t, l)) => {
                    writeln!(w.text, "  imul ${}, %rdi", t.size())?;
                    Type::Array(t, l)
                }
                (t @ Type::Pointer(_), Type::Int) => {
                    writeln!(w.text, "  imul ${}, %rax", t.size())?;
                    t
                }
                (Type::Array(t, l), Type::Int) => {
                    writeln!(w.text, "  imul ${}, %rax", t.size())?;
                    Type::Array(t, l)
                }
                _ => panic!("invalid operation"),
            };
            writeln!(w.text, "  add %rdi, %rax")?;
            t
        }
        Node::Sub(lhs, rhs) => match gen(w, &*rhs, &*lhs, ctx)? {
            (Type::Char, Type::Char) => {
                writeln!(w.text, "  sub %rdi, %rax")?;
                Type::Char
            }
            (Type::Int, Type::Int) | (Type::Int, Type::Char) | (Type::Char, Type::Int) => {
                writeln!(w.text, "  sub %rdi, %rax")?;
                Type::Int
            }
            (Type::Int, t @ Type::Pointer(_)) => {
                writeln!(w.text, "  imul ${}, %rdi", t.size())?;
                writeln!(w.text, "  sub %rdi, %rax")?;
                t
            }
            (Type::Int, Type::Array(t, l)) => {
                writeln!(w.text, "  imul ${}, %rdi", t.size())?;
                writeln!(w.text, "  sub %rdi, %rax")?;
                Type::Array(t, l)
            }
            (Type::Pointer(_), Type::Pointer(_))
            | (Type::Pointer(_), Type::Array(_, _))
            | (Type::Array(_, _), Type::Pointer(_))
            | (Type::Array(_, _), Type::Array(_, _)) => {
                writeln!(w.text, "  sub %rdi, %rax")?;
                writeln!(
                    w.text,
                    "  mov ${}, %rdi",
                    Type::Pointer(Box::new(Type::Int)).size()
                )?;
                writeln!(w.text, "  cqo")?;
                writeln!(w.text, "  idiv %rdi")?;
                Type::Int
            }
            _ => {
                panic!("invalid operation")
            }
        },
        Node::Mul(lhs, rhs) => {
            gen(w, &*rhs, &*lhs, ctx)?;
            writeln!(w.text, "  imul %rdi, %rax")?;
            Type::Int
        }
        Node::Div(lhs, rhs) => {
            gen(w, &*rhs, &*lhs, ctx)?;
            writeln!(w.text, "  cqo")?;
            writeln!(w.text, "  idiv %rdi")?;
            Type::Int
        }
        Node::Eq(lhs, rhs) => {
            cmp(w, &*rhs, &*lhs, "sete", ctx)?;
            Type::Int
        }
        Node::Ne(lhs, rhs) => {
            cmp(w, &*rhs, &*lhs, "setne", ctx)?;
            Type::Int
        }
        Node::Lt(lhs, rhs) => {
            cmp(w, &*rhs, &*lhs, "setl", ctx)?;
            Type::Int
        }
        Node::Le(lhs, rhs) => {
            cmp(w, &*rhs, &*lhs, "setle", ctx)?;
            Type::Int
        }
        Node::Sizeof(rhs) => {
            let t = generate_expr(
                &mut SectionWriter {
                    data: &mut std::io::sink(),
                    text: &mut std::io::sink(),
                },
                &*rhs,
                ctx,
            )?;
            writeln!(w.text, "  mov ${}, %rax", t.size())?;
            Type::Int
        }
    })
}

fn generate_stmt(w: &mut SectionWriter, stmt: Statement, ctx: &Context) -> std::io::Result<()> {
    match stmt {
        Statement::Block(s) => s.into_iter().try_for_each(|x| generate_stmt(w, x, ctx))?,
        Statement::Decl(decls) => {
            for x in decls {
                if let Some(init) = x.init {
                    generate_expr(w, &init, ctx)?;
                }
            }
        }
        Statement::Expr(expr) => {
            generate_expr(w, &expr, ctx)?;
        }
        Statement::Return(expr) => {
            generate_expr(w, &expr, ctx)?;
            writeln!(w.text, "  jmp .L.return.{}", ctx.funcname)?;
        }
        Statement::If { cond, then, r#else } => {
            let c = count();
            generate_expr(w, &cond, ctx)?;
            writeln!(w.text, "  cmp $0, %rax")?;
            writeln!(w.text, "  je  .L.else.{}", c)?;
            generate_stmt(w, *then, ctx)?;
            writeln!(w.text, "  jmp .L.end.{}", c)?;
            writeln!(w.text, ".L.else.{}:", c)?;

            if let Some(e) = r#else {
                generate_stmt(w, *e, ctx)?;
            }
            writeln!(w.text, ".L.end.{}:", c)?;
        }
        Statement::Iter {
            init,
            cond,
            next,
            then,
        } => {
            let c = count();
            if let Some(init) = init {
                generate_expr(w, &init, ctx)?;
            }
            writeln!(w.text, ".L.begin.{}:", c)?;
            if let Some(cond) = cond {
                generate_expr(w, &cond, ctx)?;
                writeln!(w.text, "  cmp $0, %rax")?;
                writeln!(w.text, "  je  .L.end.{}", c)?;
            }
            generate_stmt(w, *then, ctx)?;
            if let Some(next) = next {
                generate_expr(w, &next, ctx)?;
            }
            writeln!(w.text, "  jmp .L.begin.{}", c)?;
            writeln!(w.text, ".L.end.{}:", c)?;
        }
    }
    Ok(())
}

fn generate_definition(
    w: &mut SectionWriter,
    global: &mut Context,
    def: Definition,
) -> std::io::Result<()> {
    match def {
        Definition::Function {
            typ: _,
            name,
            params,
            body,
        } => {
            let nparams = params.len();
            let mut variables: Vec<(Declaration, i64)> = params
                .into_iter()
                .rev()
                .enumerate()
                .map(|(i, x)| (x, i as i64 * 8))
                .collect();

            find_variable(&mut variables, &body);
            let stack_size = variables
                .last()
                .map(|(d, o)| d.typ.size() as i64 + o)
                .unwrap_or(0);
            for (_, v) in variables.iter_mut() {
                *v -= stack_size;
            }

            writeln!(w.text, "  .globl {}", name)?;
            writeln!(w.text, "  .text")?;
            writeln!(w.text, "{}:", name)?;

            writeln!(w.text, "  push %rbp")?;
            writeln!(w.text, "  mov %rsp, %rbp")?;
            writeln!(w.text, "  sub ${}, %rsp", align_to(stack_size, 16))?;

            for (i, var) in variables[0..nparams].iter().rev().enumerate() {
                if var.0.typ.size() == 1 {
                    writeln!(w.text, "  mov {}, {}(%rbp)", ARGREGS8[i], var.1)?;
                } else {
                    writeln!(w.text, "  mov {}, {}(%rbp)", ARGREGS64[i], var.1)?;
                }
            }

            generate_stmt(
                &mut *w,
                *body,
                &Context {
                    parent: Some(global),
                    funcname: &name,
                    variables,
                },
            )?;

            writeln!(w.text, ".L.return.{}:", name)?;
            writeln!(w.text, "  mov %rbp, %rsp")?;
            writeln!(w.text, "  pop %rbp")?;
            writeln!(w.text, "  ret")?;
        }
        Definition::Declaration(xs) => {
            for decl @ Declaration { typ, name, init: _ } in &xs {
                global.add_variable(decl);
                writeln!(w.data, "  .data")?;
                writeln!(w.data, "  .globl {}", name)?;
                writeln!(w.data, "{}:", name)?;
                writeln!(w.data, "  .zero {}", typ.size())?;
            }
        }
    }
    Ok(())
}

pub fn generate(defs: Vec<Definition>) -> std::io::Result<()> {
    let mut text = Vec::new();
    let mut data = Vec::new();
    let mut writer = SectionWriter {
        text: &mut text,
        data: &mut data,
    };
    let mut global_ctx = Context::new(None, "");

    for def in defs {
        generate_definition(&mut writer, &mut global_ctx, def)?;
    }

    let mut out = std::io::stdout();
    out.write_all(&data)?;
    out.write_all(&text)?;

    Ok(())
}
