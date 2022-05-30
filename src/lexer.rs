use std::ops::Deref;
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, digit1, multispace0};
use nom::combinator::{eof, map, map_res, recognize, value};
use nom::error::Error;
use nom::error::ParseError;
use nom::multi::{many0, many0_count};
use nom::sequence::{delimited, pair};
use nom::{Finish, IResult, InputLength, Parser};
use nom_locate::{position, LocatedSpan};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eof,
    Ident(String),
    Integer(i64),
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    SemiColon,
    Return,
}

impl<'a> InputLength for Token {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Debug)]
pub struct SToken<'a> {
    pub position: Span<'a>,
    pub value: Token,
}

impl<'a> Deref for SToken<'a> {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

fn stoken<'a, E, F>(mut parser: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, SToken<'a>, E>
where
    F: Parser<Span<'a>, Token, E>,
    E: ParseError<Span<'a>>,
{
    move |i: Span<'a>| {
        let (i, position) = position(i)?;
        let (i, value) = parser.parse(i)?;
        Ok((i, SToken { position, value }))
    }
}

fn keyword(input: Span) -> IResult<Span, SToken> {
    stoken(value(Token::Return, tag("return")))(input)
}

fn identifier(input: Span) -> IResult<Span, SToken> {
    stoken(map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |x: Span| Token::Ident(x.fragment().to_string()),
    ))(input)
}

fn integer(input: Span) -> IResult<Span, SToken> {
    stoken(map(
        map_res(digit1, |x: Span| FromStr::from_str(x.fragment())),
        Token::Integer,
    ))(input)
}

fn punctuator(input: Span) -> IResult<Span, SToken> {
    let f = |s, t| stoken(value(t, tag(s)));

    alt((
        f("+", Token::Plus),
        f("-", Token::Minus),
        f("*", Token::Multiply),
        f("/", Token::Divide),
        f("(", Token::LParen),
        f(")", Token::RParen),
        f("==", Token::Equal),
        f("=", Token::Assign),
        f("!=", Token::NotEqual),
        f("<=", Token::LessThanEqual),
        f("<", Token::LessThan),
        f(">=", Token::GreaterThanEqual),
        f(">", Token::GreaterThan),
        f(";", Token::SemiColon),
    ))(input)
}

fn token(input: Span) -> IResult<Span, SToken> {
    alt((keyword, identifier, integer, punctuator))(input)
}

fn tokens(input: Span) -> IResult<Span, Vec<SToken>> {
    let (i, mut tokens) = many0(delimited(multispace0, token, multispace0))(input)?;
    let (i, t) = stoken(value(Token::Eof, eof))(i)?;

    tokens.push(t);
    Ok((i, tokens))
}

pub fn lex(s: &str) -> Result<(Span, Vec<SToken>), Error<Span>> {
    tokens(Span::new(s)).finish()
}
