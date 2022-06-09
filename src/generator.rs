use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use once_cell::sync::Lazy;

use crate::parser::{Declaration, Definition, Node, Statement, Type};

static VARIABLES: Lazy<Mutex<Vec<(Declaration, i64)>>> = Lazy::new(|| Mutex::new(vec![]));

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

fn find_variable(stmt: &Statement) -> i64 {
    fn find(decl: &Declaration) -> i64 {
        let mut vars = VARIABLES.lock().unwrap();
        let offset = 8 * vars.len() as i64;
        vars.push((decl.clone(), offset));
        offset + 8
    }

    match stmt {
        Statement::Block(block) => block.iter().map(find_variable).max().unwrap_or(0),
        Statement::Decl(decls) => decls.iter().map(find).max().unwrap_or(0),
        _ => 0,
    }
}

fn generate_expr(node: &Node) -> Type {
    fn gen(rhs: &Node, lhs: &Node) -> (Type, Type) {
        let rt = generate_expr(rhs);
        push();
        let lt = generate_expr(lhs);
        pop("%rdi");
        (rt, lt)
    }

    fn gen_addr(name: &str) {
        let vars = VARIABLES.lock().unwrap();
        let (_, offset) = vars
            .iter()
            .rev()
            .find(|(decl, _)| decl.name == name)
            .unwrap();
        println!("  lea {}(%rbp), %rax", offset);
    }

    fn cmp(rhs: &Node, lhs: &Node, op: &str) {
        gen(rhs, lhs);
        println!("  cmp %rdi, %rax");
        println!("  {} %al", op);
        println!("  movzb %al, %rax");
    }

    match node {
        Node::Assign(lhs, rhs) => {
            match &**lhs {
                Node::Var(name) => {
                    gen_addr(name);
                }
                Node::Deref(lhs) => {
                    generate_expr(&*lhs);
                }
                _ => {
                    panic!("unexpedted lhs of assign")
                }
            }
            push();
            let t = generate_expr(&*rhs);
            pop("%rdi");
            println!("  mov %rax, (%rdi)");
            t
        }
        Node::Call(name, args) => {
            for arg in args.iter() {
                generate_expr(arg);
                push();
            }

            for reg in ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"][0..args.len()]
                .iter()
                .rev()
            {
                pop(reg)
            }

            println!("  mov $0, %rax");
            println!("  call {}", name);
            Type::Int
        }
        Node::Var(name) => {
            gen_addr(name);
            println!("  mov (%rax), %rax");
            Type::Int
        }
        Node::Integer(i) => {
            println!("  mov ${}, %rax", i);
            Type::Int
        }
        Node::Neg(lhs) => {
            generate_expr(&*lhs);
            println!("  neg %rax");
            Type::Int
        }
        Node::Deref(lhs) => {
            let t = generate_expr(&*lhs);
            println!("  mov (%rax), %rax");
            t
        }
        Node::Addr(lhs) => match &**lhs {
            Node::Var(name) => {
                gen_addr(name);
                let vars = VARIABLES.lock().unwrap();
                let (decl, _) = vars
                    .iter()
                    .rev()
                    .find(|(decl, _)| &decl.name == name)
                    .unwrap();
                Type::Pointer(Box::new(decl.typ.clone()))
            }
            _ => panic!("unexpected lhs of addr"),
        },
        Node::Add(lhs, rhs) => {
            let t = match gen(&*rhs, &*lhs) {
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
        Node::Sub(lhs, rhs) => match gen(&*rhs, &*lhs) {
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
            gen(&*rhs, &*lhs);
            println!("  imul %rdi, %rax");
            Type::Int
        }
        Node::Div(lhs, rhs) => {
            gen(&*rhs, &*lhs);
            println!("  cqo");
            println!("  idiv %rdi");
            Type::Int
        }
        Node::Eq(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "sete");
            Type::Int
        }
        Node::Ne(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "setne");
            Type::Int
        }
        Node::Lt(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "setl");
            Type::Int
        }
        Node::Le(lhs, rhs) => {
            cmp(&*rhs, &*lhs, "setle");
            Type::Int
        }
    }
}

fn generate_stmt(stmt: Statement, funcname: &str) {
    match stmt {
        Statement::Block(s) => {
            s.into_iter().for_each(|x| generate_stmt(x, funcname));
        }
        Statement::Decl(decls) => {
            for x in decls {
                if let Some(init) = x.init {
                    generate_expr(&init);
                }
            }
        }
        Statement::Expr(expr) => {
            generate_expr(&expr);
        }
        Statement::Return(expr) => {
            generate_expr(&expr);
            println!("  jmp .L.return.{}", funcname);
        }
        Statement::If { cond, then, r#else } => {
            let c = count();
            generate_expr(&cond);
            println!("  cmp $0, %rax");
            println!("  je  .L.else.{}", c);
            generate_stmt(*then, funcname);
            println!("  jmp .L.end.{}", c);
            println!(".L.else.{}:", c);

            if let Some(e) = r#else {
                generate_stmt(*e, funcname);
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
                generate_expr(&init);
            }
            println!(".L.begin.{}:", c);
            if let Some(cond) = cond {
                generate_expr(&cond);
                println!("  cmp $0, %rax");
                println!("  je  .L.end.{}", c);
            }
            generate_stmt(*then, funcname);
            if let Some(next) = next {
                generate_expr(&next);
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
            args: _,
            body,
        } => {
            let stack_size = find_variable(&body);
            {
                let mut vars = VARIABLES.lock().unwrap();
                for (_, v) in vars.iter_mut() {
                    *v -= stack_size;
                }
            }

            println!("  .globl {}", name);
            println!("{}:", name);

            println!("  push %rbp");
            println!("  mov %rsp, %rbp");
            println!("  sub ${}, %rsp", align_to(stack_size, 16));

            generate_stmt(*body, &name);

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
