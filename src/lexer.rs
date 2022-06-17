use std::ops::Deref;
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, anychar, digit1, multispace0, none_of};
use nom::combinator::{eof, map, map_res, recognize, value};
use nom::error::{Error, ParseError};
use nom::multi::{fold_many0, many0, many0_count};
use nom::sequence::{delimited, pair};
use nom::{Finish, IResult, InputLength, Parser};
use nom_locate::{position, LocatedSpan};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eof,
    Ident(String),
    Integer(i64),
    Str(String),
    // Keywords
    Int,
    Char,
    If,
    Else,
    For,
    While,
    Return,
    Sizeof,
    // Punctuators
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    SemiColon,
    Comma,
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
    stoken(alt((
        value(Token::Char, tag("char")),
        value(Token::Else, tag("else")),
        value(Token::For, tag("for")),
        value(Token::If, tag("if")),
        value(Token::Int, tag("int")),
        value(Token::Return, tag("return")),
        value(Token::Sizeof, tag("sizeof")),
        value(Token::While, tag("while")),
    )))(input)
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

fn character(input: Span) -> IResult<Span, char> {
    let (input, c) = none_of("\"")(input)?;
    if c == '\\' {
        map(anychar, |c| match c {
            'a' => '\x07',
            'b' => '\x08',
            'e' => '\x1B',
            'f' => '\x0C',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0B',
            _ => c,
        })(input)
    } else {
        Ok((input, c))
    }
}

fn string(input: Span) -> IResult<Span, SToken> {
    stoken(map(
        delimited(
            tag("\""),
            fold_many0(character, String::new, |mut s, c| {
                s.push(c);
                s
            }),
            tag("\""),
        ),
        Token::Str,
    ))(input)
}

fn punctuator(input: Span) -> IResult<Span, SToken> {
    let f = |s, t| stoken(value(t, tag(s)));

    alt((
        f("+", Token::Plus),
        f("-", Token::Minus),
        f("*", Token::Multiply),
        f("&", Token::And),
        f("/", Token::Divide),
        f("(", Token::LParen),
        f(")", Token::RParen),
        f("{", Token::LBrace),
        f("}", Token::RBrace),
        f("[", Token::LBracket),
        f("]", Token::RBracket),
        f("==", Token::Equal),
        f("=", Token::Assign),
        f("!=", Token::NotEqual),
        f("<=", Token::LessThanEqual),
        f("<", Token::LessThan),
        f(">=", Token::GreaterThanEqual),
        f(">", Token::GreaterThan),
        f(";", Token::SemiColon),
        f(",", Token::Comma),
    ))(input)
}

fn token(input: Span) -> IResult<Span, SToken> {
    alt((keyword, identifier, integer, string, punctuator))(input)
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
