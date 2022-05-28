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

    fn cmp(rhs: Node, lhs: Node, op: &str) {
        gen(rhs, lhs);
        println!("  cmp %rdi, %rax");
        println!("  {} %al", op);
        println!("  movzb %al, %rax");
    }

    match node {
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

    stmts.into_iter().for_each(stmt);

    println!("  ret");
}
