use std::env;
use std::str::FromStr;

fn strtol(s: &str) -> Result<(i64, &str), Box<dyn std::error::Error>> {
    if !s
        .chars()
        .nth(0)
        .ok_or("invalid number".to_string())?
        .is_digit(10)
    {
        return Err("invalid number".to_string()).map_err(|e| e.into());
    }

    match s.find(|c: char| !c.is_digit(10)) {
        Some(i) => {Ok((i64::from_str(&s[..i])?, &s[i..]))},
        None => Ok((i64::from_str(s)?, "")),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("invalid number of arguments")
    }

    println!("  .globl main");
    println!("main:");

    let (mut n, mut s) = strtol(&args[1])?;
    println!("  mov ${}, %rax", n);

    while s.len() != 0 {
        let op = match s.chars().next().unwrap() {
            '+' => "add",
            '-' => "sub",
            other => panic!("unexpected character: {}", other)
        };
        (n, s) = strtol(&s[1..])?;
        println!("  {} ${}, %rax", op, n)
    }

    println!("  ret");

    Ok(())
}
