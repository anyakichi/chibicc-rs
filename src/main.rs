use std::env;
use std::str::FromStr;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("invalid number of arguments")
    }

    println!("  .globl main");
    println!("main:");
    println!("  mov ${}, %rax", i64::from_str(&args[1])?);
    println!("  ret");

    Ok(())
}
