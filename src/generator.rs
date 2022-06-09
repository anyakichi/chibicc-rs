use std::sync::atomic::{AtomicUsize, Ordering};

use crate::parser::{Declaration, Definition, Node, Statement, Type};

static ARGREGS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

struct Context<'a> {
    funcname: &'a str,
    variables: Vec<(Declaration, i64)>,
}

fn count() -> usize {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    COUNT.fetch_add(1, Ordering::SeqCst)
}

fn align_to(n: i64, align: i64) -> i64 {
    (n + align - 1) / align * align
}

fn push() {
    println!("  push %rax");
}

fn pop(r: &str) {
    println!("  pop {}", r);
}

fn find_variable(vars: &mut Vec<(Declaration, i64)>, stmt: &Statement) {
    match stmt {
        Statement::Block(block) => {
            for stmt in block {
                find_variable(vars, stmt)
            }
        }
        Statement::Decl(decls) => {
            for decl in decls {
                let offset = 8 * vars.len() as i64;
                vars.push((decl.clone(), offset));
            }
        }
        _ => (),
    }
}

fn generate_expr(node: &Node, ctx: &Context) -> Type {
    fn gen(rhs: &Node, lhs: &Node, ctx: &Context) -> (Type, Type) {
        let rt = generate_expr(rhs, ctx);
        push();
        let lt = generate_expr(lhs, ctx);
        pop("%rdi");
        (rt, lt)
    }

    fn gen_addr(name: &str, vars: &[(Declaration, i64)]) {
        let (_, offset) = vars
            .iter()
            .rev()
            .find(|(decl, _)| decl.name == name)
            .unwrap();
        println!("  lea {}(%rbp), %rax", offset);
    }

    fn cmp(rhs: &Node, lhs: &Node, op: &str, ctx: &Context) {
        gen(rhs, lhs, ctx);
        println!("  cmp %rdi, %rax");
        println!("  {} %al", op);
        println!("  movzb %al, %rax");
    }

    match node {
        Node::Assign(lhs, rhs) => {
            match &**lhs {
                Node::Var(name) => {
                    gen_addr(name, &ctx.variables);
                }
                Node::Deref(lhs) => {
                    generate_expr(&*lhs, ctx);
                }
                _ => {
                    panic!("unexpedted lhs of assign")
                }
            }
            push();
            let t = generate_expr(&*rhs, ctx);
            pop("%rdi");
            println!("  mov %rax, (%rdi)");
            t
        }
        Node::Call(name, args) => {
            for arg in args.iter() {
                generate_expr(arg, ctx);
                push();
            }

            for reg in ARGREGS[0..args.len()].iter().rev() {
                pop(reg)
            }

            println!("  mov $0, %rax");
            println!("  call {}", name);
            Type::Int
        }
        Node::Var(name) => {
            gen_addr(name, &ctx.variables);
            println!("  mov (%rax), %rax");
            Type::Int
        }
        Node::Integer(i) => {
            println!("  mov ${}, %rax", i);
            Type::Int
        }
        Node::Neg(lhs) => {
            generate_expr(&*lhs, ctx);
            println!("  neg %rax");
            Type::Int
        }
        Node::Deref(lhs) => {
            let t = generate_expr(&*lhs, ctx);
            println!("  mov (%rax), %rax");
            t
        }
        Node::Addr(lhs) => match &**lhs {
            Node::Var(name) => {
                gen_addr(name, &ctx.variables);
                let (decl, _) = ctx
                    .variables
                    .iter()
                    .rev()
                    .find(|(decl, _)| &decl.name == name)
                    .unwrap();
                Type::Pointer(Box::new(decl.typ.clone()))
            }
            _ => panic!("unexpected lhs of addr"),
        },
        Node::Add(lhs, rhs) => {
            let t = match gen(&*rhs, &*lhs, ctx) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Int, Type::Pointer(t)) => {
                    println!("  imul $8, %rdi");
                    Type::Pointer(t)
                }
                (Type::Pointer(t), Type::Int) => {
                    println!("  imul $8, %rax");
                    Type::Pointer(t)
                }
                _ => panic!("invalid operation"),
            };
            println!("  add %rdi, %rax");
            t
        }
        Node::Sub(lhs, rhs) => match gen(&*rhs, &*lhs, ctx) {
            (Type::Int, Type::Int) => {
                println!("  sub %rdi, %rax");
                Type::Int
            }
            (Type::Int, Type::Pointer(t)) => {
                println!("  imul $8, %rdi");
                println!("  sub %rdi, %rax");
                Type::Pointer(t)
            }
            (Type::Pointer(_), Type::Pointer(_)) => {
                println!("  sub %rdi, %rax");
                println!("  mov $8, %rdi");
                println!("  cqo");
                println!("  idiv %rdi");
                Type::Int
            }
            _ => {
                panic!("invalid operation")
            }
        },
        Node::Mul(lhs, rhs) => {
            gen(&*rhs, &*lhs, ctx);
            println!("  imul %rdi, %rax");
            Type::Int
        }
        Node::Div(lhs, rhs) => {
            gen(&*rhs, &*lhs, ctx);
            println!("  cqo");
            println!("  idiv %rdi");
            Type::Int
        }
        Node::Eq(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "sete", ctx);
            Type::Int
        }
        Node::Ne(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "setne", ctx);
            Type::Int
        }
        Node::Lt(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "setl", ctx);
            Type::Int
        }
        Node::Le(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "setle", ctx);
            Type::Int
        }
    }
}

fn generate_stmt(stmt: Statement, ctx: &Context) {
    match stmt {
        Statement::Block(s) => {
            s.into_iter().for_each(|x| generate_stmt(x, ctx));
        }
        Statement::Decl(decls) => {
            for x in decls {
                if let Some(init) = x.init {
                    generate_expr(&init, ctx);
                }
            }
        }
        Statement::Expr(expr) => {
            generate_expr(&expr, ctx);
        }
        Statement::Return(expr) => {
            generate_expr(&expr, ctx);
            println!("  jmp .L.return.{}", ctx.funcname);
        }
        Statement::If { cond, then, r#else } => {
            let c = count();
            generate_expr(&cond, ctx);
            println!("  cmp $0, %rax");
            println!("  je  .L.else.{}", c);
            generate_stmt(*then, ctx);
            println!("  jmp .L.end.{}", c);
            println!(".L.else.{}:", c);

            if let Some(e) = r#else {
                generate_stmt(*e, ctx);
            }
            println!(".L.end.{}:", c);
        }
        Statement::Iter {
            init,
            cond,
            next,
            then,
        } => {
            let c = count();
            if let Some(init) = init {
                generate_expr(&init, ctx);
            }
            println!(".L.begin.{}:", c);
            if let Some(cond) = cond {
                generate_expr(&cond, ctx);
                println!("  cmp $0, %rax");
                println!("  je  .L.end.{}", c);
            }
            generate_stmt(*then, ctx);
            if let Some(next) = next {
                generate_expr(&next, ctx);
            }
            println!("  jmp .L.begin.{}", c);
            println!(".L.end.{}:", c);
        }
    }
}

fn generate_definition(def: Definition) {
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
            let stack_size = variables.len() as i64 * 8;
            for (_, v) in variables.iter_mut() {
                *v -= stack_size;
            }

            println!("  .globl {}", name);
            println!("{}:", name);

            println!("  push %rbp");
            println!("  mov %rsp, %rbp");
            println!("  sub ${}, %rsp", align_to(stack_size, 16));

            for (i, var) in variables[0..nparams].iter().rev().enumerate() {
                println!("  mov {}, {}(%rbp)", ARGREGS[i], var.1);
            }

            generate_stmt(
                *body,
                &Context {
                    funcname: &name,
                    variables,
                },
            );

            println!(".L.return.{}:", name);
            println!("  mov %rbp, %rsp");
            println!("  pop %rbp");
            println!("  ret");
        }
        Definition::Declaration(_) => {
            // TODO
        }
    }
}

pub fn generate(defs: Vec<Definition>) {
    for def in defs {
        generate_definition(def);
    }
}
