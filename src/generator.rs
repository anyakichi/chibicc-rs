use std::collections::HashMap;
use std::sync::Mutex;

use once_cell::sync::Lazy;

use crate::parser::Node;

static VARIABLES: Lazy<Mutex<HashMap<String, i64>>> = Lazy::new(|| Mutex::new(HashMap::new()));

fn align_to(n: i64, align: i64) -> i64 {
    (n + align - 1) / align * align
}

fn push() {
    println!("  push %rax");
}

fn pop(r: &str) {
    println!("  pop {}", r);
}

fn find_variable(node: &Node) -> i64 {
    match node {
        Node::Block(block) => block.iter().map(find_variable).max().unwrap_or(0),
        Node::Assign(lhs, rhs) => {
            let a = find_variable(&*lhs);
            let b = find_variable(&*rhs);
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

fn generate1(node: Node) {
    fn gen(rhs: Node, lhs: Node) {
        generate1(rhs);
        push();
        generate1(lhs);
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
            if let Node::Var(name) = *lhs {
                gen_addr(&name);
            } else {
                panic!("unexpedted lhs of assign")
            }
            push();
            generate1(*rhs);
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
            generate1(*lhs);
            println!("  neg %rax");
        }
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
        Node::Return(rhs) => {
            generate1(*rhs);
            println!("  jmp .L.return");
        }
        Node::Block(rhs) => {
            rhs.into_iter().for_each(generate1);
        }
    }
}

pub fn generate(node: Node) {
    let stack_size = find_variable(&node);
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

    generate1(node);

    println!(".L.return:");
    println!("  mov %rbp, %rsp");
    println!("  pop %rbp");
    println!("  ret");
}
