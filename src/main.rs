use std::env;

use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, multispace0};
use nom::combinator::map_res;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::{Finish, IResult};

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Eof,
    Integer(i64),
    Plus,
    Minus,
}

fn integer(input: &str) -> IResult<&str, Token> {
    let (input, value) = map_res(digit1, |s: &str| s.parse::<i64>())(input)?;

    Ok((input, Token::Integer(value)))
}

fn plus(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("+")(input)?;

    Ok((input, Token::Plus))
}

fn minus(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("-")(input)?;

    Ok((input, Token::Minus))
}

fn lex(input: &str) -> IResult<&str, Token> {
    alt((integer, plus, minus))(input)
}

fn lex_tokens(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, mut tokens) = many1(delimited(multispace0, lex, multispace0))(input)?;
    if input.is_empty() {
        tokens.push(Token::Eof);
    }
    Ok((input, tokens))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("invalid number of arguments")
    }

    println!("  .globl main");
    println!("main:");

    let (_, tokens) = lex_tokens(&args[1]).finish().unwrap();
    let mut iter = tokens.iter();
    if let Some(Token::Integer(i)) = iter.next() {
        println!("  mov ${}, %rax", i);
    }
    while let Some(token) = iter.next() {
        match token {
            Token::Integer(i) => println!("  mov ${}, %rax", i),
            Token::Plus => {
                if let Some(Token::Integer(i)) = iter.next() {
                    println!("  add ${}, %rax", i)
                }
            }
            Token::Minus => {
                if let Some(Token::Integer(i)) = iter.next() {
                    println!("  sub ${}, %rax", i)
                }
            }
            Token::Eof => println!("  ret"),
        }
    }

    Ok(())
}
