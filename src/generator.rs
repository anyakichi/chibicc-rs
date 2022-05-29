use crate::parser::Node;

fn push() {
    println!("  push %rax");
}

fn pop(r: &str) {
    println!("  pop {}", r);
}

fn stmt(node: Node) {
    fn gen(rhs: Node, lhs: Node) {
        stmt(rhs);
        push();
        stmt(lhs);
        pop("%rdi");
    }

    fn gen_addr(name: char) {
        let offset = (name as i64 - 'a' as i64 + 1) * 8;
        println!("  lea {}(%rbp), %rax", -offset);
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
                gen_addr(name);
            } else {
                panic!("unexpedted lhs of assign")
            }
            push();
            stmt(*rhs);
            pop("%rdi");
            println!("  mov %rax, (%rdi)");
        }
        Node::Var(name) => {
            gen_addr(name);
            println!("  mov (%rax), %rax");
        }
        Node::Integer(i) => {
            println!("  mov ${}, %rax", i)
        }
        Node::Neg(lhs) => {
            stmt(*lhs);
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
    }
}

pub fn generate(stmts: Vec<Node>) {
    println!("  .globl main");
    println!("main:");

    println!("  push %rbp");
    println!("  mov %rsp, %rbp");
    println!("  sub $208, %rsp");

    stmts.into_iter().for_each(stmt);

    println!("  mov %rbp, %rsp");
    println!("  pop %rbp");
    println!("  ret");
}
