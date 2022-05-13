use std::env;

use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, multispace0};
use nom::multi::many1;
use nom::sequence::delimited;
use nom::{Finish, IResult};
use nom_locate::{position, LocatedSpan};

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Eof,
    Integer(i64),
    Plus,
    Minus,
}

type Span<'a> = LocatedSpan<&'a str>;

struct SToken<'a> {
    pub position: Span<'a>,
    pub value: Token,
}

fn integer(input: Span) -> IResult<Span, SToken> {
    let (input, value) = digit1(input)?;
    let (input, position) = position(input)?;

    Ok((
        input,
        SToken {
            position,
            value: Token::Integer(value.fragment().parse::<i64>().unwrap()),
        },
    ))
}

fn plus(input: Span) -> IResult<Span, SToken> {
    let (input, _) = tag("+")(input)?;
    let (input, position) = position(input)?;

    Ok((
        input,
        SToken {
            position,
            value: Token::Plus,
        },
    ))
}

fn minus(input: Span) -> IResult<Span, SToken> {
    let (input, _) = tag("-")(input)?;
    let (input, position) = position(input)?;

    Ok((
        input,
        SToken {
            position,
            value: Token::Minus,
        },
    ))
}

fn lex(input: Span) -> IResult<Span, SToken> {
    alt((integer, plus, minus))(input)
}

fn lex_tokens(input: Span) -> IResult<Span, Vec<SToken>> {
    let (input, mut tokens) = many1(delimited(multispace0, lex, multispace0))(input)?;
    if input.is_empty() {
        let (_, position) = position(input)?;
        tokens.push(SToken {
            position,
            value: Token::Eof,
        });
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

    let input = Span::new(&args[1]);
    let (_, tokens) = lex_tokens(input).finish().unwrap();
    let mut iter = tokens.iter();
    while let Some(SToken { position, value }) = iter.next() {
        match value {
            Token::Integer(i) => println!("  mov ${}, %rax", i),
            Token::Plus | Token::Minus => match iter.next() {
                Some(SToken {
                    value: Token::Integer(i),
                    ..
                }) => {
                    let op = if value == &Token::Plus { "add" } else { "sub" };
                    println!("  {} ${}, %rax", op, i)
                }
                _ => {
                    println!(
                        "{}\n{}^ expected a number",
                        &args[1],
                        " ".repeat(position.get_utf8_column() - 1)
                    );
                    std::process::exit(1)
                }
            },
            Token::Eof => println!("  ret"),
        }
    }

    Ok(())
}
