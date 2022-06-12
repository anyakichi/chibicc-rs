use std::io::Write;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::parser::{Declaration, Definition, Node, Statement, Type};

static ARGREGS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

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

fn push(writer: &mut dyn Write) -> std::io::Result<()> {
    writeln!(writer, "  push %rax")
}

fn pop(writer: &mut dyn Write, r: &str) -> std::io::Result<()> {
    writeln!(writer, "  pop {}", r)
}

fn load(writer: &mut dyn Write, typ: &Type) -> std::io::Result<()> {
    match typ {
        Type::Array(_, _) => Ok(()),
        _ => {
            writeln!(writer, "  mov (%rax), %rax")
        }
    }
}

fn store(writer: &mut dyn Write) -> std::io::Result<()> {
    pop(writer, "%rdi")?;
    writeln!(writer, "  mov %rax, (%rdi)")
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

fn generate_expr(writer: &mut dyn Write, node: &Node, ctx: &Context) -> std::io::Result<Type> {
    fn gen(
        writer: &mut dyn Write,
        rhs: &Node,
        lhs: &Node,
        ctx: &Context,
    ) -> std::io::Result<(Type, Type)> {
        let rt = generate_expr(writer, rhs, ctx)?;
        push(writer)?;
        let lt = generate_expr(writer, lhs, ctx)?;
        pop(writer, "%rdi")?;
        Ok((rt, lt))
    }

    fn gen_addr(writer: &mut dyn Write, name: &str, ctx: &Context) -> std::io::Result<()> {
        let offset = ctx.get_variable_offset(name).unwrap();
        if offset != 0 {
            writeln!(writer, "  lea {}(%rbp), %rax", offset)
        } else {
            writeln!(writer, "  lea {}(%rip), %rax", name)
        }
    }

    fn cmp(
        writer: &mut dyn Write,
        rhs: &Node,
        lhs: &Node,
        op: &str,
        ctx: &Context,
    ) -> std::io::Result<()> {
        gen(writer, rhs, lhs, ctx)?;
        writeln!(writer, "  cmp %rdi, %rax")?;
        writeln!(writer, "  {} %al", op)?;
        writeln!(writer, "  movzb %al, %rax")
    }

    Ok(match node {
        Node::Assign(lhs, rhs) => {
            match &**lhs {
                Node::Var(name) => {
                    gen_addr(writer, name, ctx)?;
                }
                Node::Deref(lhs) => {
                    generate_expr(writer, &*lhs, ctx)?;
                }
                _ => {
                    panic!("unexpedted lhs of assign")
                }
            }
            push(writer)?;
            let t = generate_expr(writer, &*rhs, ctx)?;
            store(writer)?;
            t
        }
        Node::Call(name, args) => {
            for arg in args.iter() {
                generate_expr(writer, arg, ctx)?;
                push(writer)?;
            }

            for reg in ARGREGS[0..args.len()].iter().rev() {
                pop(writer, reg)?;
            }

            writeln!(writer, "  mov $0, %rax")?;
            writeln!(writer, "  call {}", name)?;
            Type::Int
        }
        Node::Var(name) => {
            let t = &ctx.get_variable_declaration(name).unwrap().typ;
            gen_addr(writer, name, ctx)?;
            load(writer, t)?;
            t.clone()
        }
        Node::Integer(i) => {
            writeln!(writer, "  mov ${}, %rax", i)?;
            Type::Int
        }
        Node::Neg(lhs) => {
            generate_expr(writer, &*lhs, ctx)?;
            writeln!(writer, "  neg %rax")?;
            Type::Int
        }
        Node::Deref(lhs) => match generate_expr(writer, &*lhs, ctx)? {
            Type::Pointer(t) | Type::Array(t, _) => {
                load(writer, &*t)?;
                *t
            }
            _ => panic!("invalid type for deref"),
        },
        Node::Addr(lhs) => match &**lhs {
            Node::Var(name) => {
                gen_addr(writer, name, ctx)?;
                Type::Pointer(Box::new(
                    ctx.get_variable_declaration(name).unwrap().typ.clone(),
                ))
            }
            _ => panic!("unexpected lhs of addr"),
        },
        Node::Add(lhs, rhs) => {
            let t = match gen(writer, &*rhs, &*lhs, ctx)? {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Int, t @ Type::Pointer(_)) => {
                    writeln!(writer, "  imul ${}, %rdi", t.size())?;
                    t
                }
                (Type::Int, Type::Array(t, l)) => {
                    writeln!(writer, "  imul ${}, %rdi", t.size())?;
                    Type::Array(t, l)
                }
                (t @ Type::Pointer(_), Type::Int) => {
                    writeln!(writer, "  imul ${}, %rax", t.size())?;
                    t
                }
                (Type::Array(t, l), Type::Int) => {
                    writeln!(writer, "  imul ${}, %rax", t.size())?;
                    Type::Array(t, l)
                }
                _ => panic!("invalid operation"),
            };
            writeln!(writer, "  add %rdi, %rax")?;
            t
        }
        Node::Sub(lhs, rhs) => match gen(writer, &*rhs, &*lhs, ctx)? {
            (Type::Int, Type::Int) => {
                writeln!(writer, "  sub %rdi, %rax")?;
                Type::Int
            }
            (Type::Int, t @ Type::Pointer(_)) => {
                writeln!(writer, "  imul ${}, %rdi", t.size())?;
                writeln!(writer, "  sub %rdi, %rax")?;
                t
            }
            (Type::Int, Type::Array(t, l)) => {
                writeln!(writer, "  imul ${}, %rdi", t.size())?;
                writeln!(writer, "  sub %rdi, %rax")?;
                Type::Array(t, l)
            }
            (Type::Pointer(_), Type::Pointer(_))
            | (Type::Pointer(_), Type::Array(_, _))
            | (Type::Array(_, _), Type::Pointer(_))
            | (Type::Array(_, _), Type::Array(_, _)) => {
                writeln!(writer, "  sub %rdi, %rax")?;
                writeln!(
                    writer,
                    "  mov ${}, %rdi",
                    Type::Pointer(Box::new(Type::Int)).size()
                )?;
                writeln!(writer, "  cqo")?;
                writeln!(writer, "  idiv %rdi")?;
                Type::Int
            }
            _ => {
                panic!("invalid operation")
            }
        },
        Node::Mul(lhs, rhs) => {
            gen(writer, &*rhs, &*lhs, ctx)?;
            writeln!(writer, "  imul %rdi, %rax")?;
            Type::Int
        }
        Node::Div(lhs, rhs) => {
            gen(writer, &*rhs, &*lhs, ctx)?;
            writeln!(writer, "  cqo")?;
            writeln!(writer, "  idiv %rdi")?;
            Type::Int
        }
        Node::Eq(lhs, rhs) => {
            cmp(writer, &*rhs, &*lhs, "sete", ctx)?;
            Type::Int
        }
        Node::Ne(lhs, rhs) => {
            cmp(writer, &*rhs, &*lhs, "setne", ctx)?;
            Type::Int
        }
        Node::Lt(lhs, rhs) => {
            cmp(writer, &*rhs, &*lhs, "setl", ctx)?;
            Type::Int
        }
        Node::Le(lhs, rhs) => {
            cmp(writer, &*rhs, &*lhs, "setle", ctx)?;
            Type::Int
        }
        Node::Sizeof(rhs) => {
            let t = generate_expr(&mut std::io::sink(), &*rhs, ctx)?;
            writeln!(writer, "  mov ${}, %rax", t.size())?;
            Type::Int
        }
    })
}

fn generate_stmt(writer: &mut dyn Write, stmt: Statement, ctx: &Context) -> std::io::Result<()> {
    match stmt {
        Statement::Block(s) => s
            .into_iter()
            .try_for_each(|x| generate_stmt(writer, x, ctx))?,
        Statement::Decl(decls) => {
            for x in decls {
                if let Some(init) = x.init {
                    generate_expr(writer, &init, ctx)?;
                }
            }
        }
        Statement::Expr(expr) => {
            generate_expr(writer, &expr, ctx)?;
        }
        Statement::Return(expr) => {
            generate_expr(writer, &expr, ctx)?;
            writeln!(writer, "  jmp .L.return.{}", ctx.funcname)?;
        }
        Statement::If { cond, then, r#else } => {
            let c = count();
            generate_expr(writer, &cond, ctx)?;
            writeln!(writer, "  cmp $0, %rax")?;
            writeln!(writer, "  je  .L.else.{}", c)?;
            generate_stmt(writer, *then, ctx)?;
            writeln!(writer, "  jmp .L.end.{}", c)?;
            writeln!(writer, ".L.else.{}:", c)?;

            if let Some(e) = r#else {
                generate_stmt(writer, *e, ctx)?;
            }
            writeln!(writer, ".L.end.{}:", c)?;
        }
        Statement::Iter {
            init,
            cond,
            next,
            then,
        } => {
            let c = count();
            if let Some(init) = init {
                generate_expr(writer, &init, ctx)?;
            }
            writeln!(writer, ".L.begin.{}:", c)?;
            if let Some(cond) = cond {
                generate_expr(writer, &cond, ctx)?;
                writeln!(writer, "  cmp $0, %rax")?;
                writeln!(writer, "  je  .L.end.{}", c)?;
            }
            generate_stmt(writer, *then, ctx)?;
            if let Some(next) = next {
                generate_expr(writer, &next, ctx)?;
            }
            writeln!(writer, "  jmp .L.begin.{}", c)?;
            writeln!(writer, ".L.end.{}:", c)?;
        }
    }
    Ok(())
}

fn generate_definition(global: &mut Context, def: Definition) -> std::io::Result<()> {
    let mut writer = std::io::stdout();

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

            writeln!(writer, "  .globl {}", name)?;
            writeln!(writer, "  .text")?;
            writeln!(writer, "{}:", name)?;

            writeln!(writer, "  push %rbp")?;
            writeln!(writer, "  mov %rsp, %rbp")?;
            writeln!(writer, "  sub ${}, %rsp", align_to(stack_size, 16))?;

            for (i, var) in variables[0..nparams].iter().rev().enumerate() {
                writeln!(writer, "  mov {}, {}(%rbp)", ARGREGS[i], var.1)?;
            }

            generate_stmt(
                &mut writer,
                *body,
                &Context {
                    parent: Some(global),
                    funcname: &name,
                    variables,
                },
            )?;

            writeln!(writer, ".L.return.{}:", name)?;
            writeln!(writer, "  mov %rbp, %rsp")?;
            writeln!(writer, "  pop %rbp")?;
            writeln!(writer, "  ret")?;
        }
        Definition::Declaration(xs) => {
            for decl @ Declaration { typ, name, init: _ } in &xs {
                global.add_variable(decl);
                writeln!(writer, "  .data")?;
                writeln!(writer, "  .globl {}", name)?;
                writeln!(writer, "{}:", name)?;
                writeln!(writer, "  .zero {}", typ.size())?;
            }
        }
    }
    Ok(())
}

pub fn generate(defs: Vec<Definition>) -> std::io::Result<()> {
    let mut global_ctx = Context::new(None, "");

    for def in defs {
        generate_definition(&mut global_ctx, def)?;
    }
    Ok(())
}
