use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use once_cell::sync::Lazy;

use crate::parser::{Node, Statement};

static VARIABLES: Lazy<Mutex<HashMap<String, i64>>> = Lazy::new(|| Mutex::new(HashMap::new()));

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
    fn find_in_expr(node: &Node) -> i64 {
        match node {
            Node::Assign(lhs, rhs) => {
                let a = find_in_expr(&*lhs);
                let b = find_in_expr(&*rhs);
                a.max(b)
            }
            Node::Var(name) => {
                let mut vars = VARIABLES.lock().unwrap();
                let offset = 8 * vars.len() as i64;
                *vars.entry(name.to_string()).or_insert(offset) + 8
            }
            _ => 0,
        }
    }

    match stmt {
        Statement::Block(block) => block.iter().map(find_variable).max().unwrap_or(0),
        Statement::Expr(expr) => find_in_expr(expr),
        _ => 0,
    }
}

fn generate_expr(node: Node) {
    fn gen(rhs: Node, lhs: Node) {
        generate_expr(rhs);
        push();
        generate_expr(lhs);
        pop("%rdi");
    }

    fn gen_addr(name: &str) {
        let vars = VARIABLES.lock().unwrap();
        let offset = vars.get(name).unwrap();
        println!("  lea {}(%rbp), %rax", offset);
    }

    fn cmp(rhs: Node, lhs: Node, op: &str) {
        gen(rhs, lhs);
        println!("  cmp %rdi, %rax");
        println!("  {} %al", op);
        println!("  movzb %al, %rax");
    }

    match node {
        Node::Assign(lhs, rhs) => {
            match *lhs {
                Node::Var(name) => {
                    gen_addr(&name);
                }
                Node::Deref(lhs) => {
                    generate_expr(*lhs);
                }
                _ => {
                    panic!("unexpedted lhs of assign")
                }
            }
            push();
            generate_expr(*rhs);
            pop("%rdi");
            println!("  mov %rax, (%rdi)");
        }
        Node::Var(name) => {
            gen_addr(&name);
            println!("  mov (%rax), %rax");
        }
        Node::Integer(i) => {
            println!("  mov ${}, %rax", i)
        }
        Node::Neg(lhs) => {
            generate_expr(*lhs);
            println!("  neg %rax");
        }
        Node::Deref(lhs) => {
            generate_expr(*lhs);
            println!("  mov (%rax), %rax");
        }
        Node::Addr(lhs) => match *lhs {
            Node::Var(name) => gen_addr(&name),
            _ => panic!("unexpected lhs of addr"),
        },
        Node::Add(lhs, rhs) => {
            gen(*rhs, *lhs);
            println!("  add %rdi, %rax");
        }
        Node::Sub(lhs, rhs) => {
            gen(*rhs, *lhs);
            println!("  sub %rdi, %rax");
        }
        Node::Mul(lhs, rhs) => {
            gen(*rhs, *lhs);
            println!("  imul %rdi, %rax");
        }
        Node::Div(lhs, rhs) => {
            gen(*rhs, *lhs);
            println!("  cqo");
            println!("  idiv %rdi");
        }
        Node::Eq(lhs, rhs) => {
            cmp(*rhs, *lhs, "sete");
        }
        Node::Ne(lhs, rhs) => {
            cmp(*rhs, *lhs, "setne");
        }
        Node::Lt(lhs, rhs) => {
            cmp(*rhs, *lhs, "setl");
        }
        Node::Le(lhs, rhs) => {
            cmp(*rhs, *lhs, "setle");
        }
    }
}

fn generate_stmt(stmt: Statement) {
    match stmt {
        Statement::Block(s) => {
            s.into_iter().for_each(generate_stmt);
        }
        Statement::Expr(expr) => {
            generate_expr(expr);
        }
        Statement::Return(expr) => {
            generate_expr(expr);
            println!("  jmp .L.return");
        }
        Statement::If { cond, then, r#else } => {
            let c = count();
            generate_expr(cond);
            println!("  cmp $0, %rax");
            println!("  je  .L.else.{}", c);
            generate_stmt(*then);
            println!("  jmp .L.end.{}", c);
            println!(".L.else.{}:", c);

            if let Some(e) = r#else {
                generate_stmt(*e);
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
                generate_expr(init);
            }
            println!(".L.begin.{}:", c);
            if let Some(cond) = cond {
                generate_expr(cond);
                println!("  cmp $0, %rax");
                println!("  je  .L.end.{}", c);
            }
            generate_stmt(*then);
            if let Some(next) = next {
                generate_expr(next);
            }
            println!("  jmp .L.begin.{}", c);
            println!(".L.end.{}:", c);
        }
    }
}

pub fn generate(stmt: Statement) {
    let stack_size = find_variable(&stmt);
    {
        let mut vars = VARIABLES.lock().unwrap();
        for (_, v) in vars.iter_mut() {
            *v -= stack_size;
        }
    }

    println!("  .globl main");
    println!("main:");

    println!("  push %rbp");
    println!("  mov %rsp, %rbp");
    println!("  sub ${}, %rsp", align_to(stack_size, 16));

    generate_stmt(stmt);

    println!(".L.return:");
    println!("  mov %rbp, %rsp");
    println!("  pop %rbp");
    println!("  ret");
}
