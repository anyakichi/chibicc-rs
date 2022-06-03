use std::env;

use anyhow::Result;

use chibicc::generator::generate;
use chibicc::lexer::lex;
use chibicc::parser::{parse, Tokens};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("invalid number of arguments")
    }

    let x = lex(&args[1]);
    let tokens = match x {
        Ok((_, t)) => t,
        Err(e) => {
            println!("{}", &args[1]);
            println!(
                "{}^ invalid token",
                " ".repeat(e.input.get_utf8_column() - 1)
            );
            std::process::exit(1);
        }
    };

    let tokens = Tokens::new(&tokens);
    match parse(tokens) {
        Ok((_, result)) => generate(result),
        Err(nom::Err::Error(e)) => {
            println!("{}", &args[1]);
            println!(
                "{}^ invalid token",
                " ".repeat(
                    e.input
                        .first()
                        .map(|x| x.position.get_utf8_column() - 1)
                        .unwrap_or(0)
                )
            );
            std::process::exit(1);
        }
        e => panic!("{:?}", e),
    }

    Ok(())
}
