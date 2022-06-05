use std::env;
use std::iter::Enumerate;
use std::ops::{Deref, Range, RangeFrom, RangeFull, RangeTo};
use std::str::FromStr;

use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{digit1, multispace0};
use nom::combinator::{eof, map, map_res, value};
use nom::error::{Error, ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::{delimited, pair, terminated};
use nom::{
    Compare, CompareResult, Err, Finish, IResult, InputIter, InputLength, InputTake, Needed,
    Parser, Slice,
};
use nom_locate::{position, LocatedSpan};

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token {
    Eof,
    Integer(i64),
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
}

impl<'a> InputLength for Token {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Debug)]
struct SToken<'a> {
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
    ))(input)
}

fn token(input: Span) -> IResult<Span, SToken> {
    alt((integer, punctuator))(input)
}

fn tokens(input: Span) -> IResult<Span, Vec<SToken>> {
    let (i, mut tokens) = many0(delimited(multispace0, token, multispace0))(input)?;
    let (i, t) = stoken(value(Token::Eof, eof))(i)?;

    tokens.push(t);
    Ok((i, tokens))
}

fn lex(input: Span) -> IResult<Span, Vec<SToken>> {
    tokens(input)
}

#[derive(Clone, Copy, Debug)]
struct Tokens<'a>(&'a [SToken<'a>]);

impl<'a> Deref for Tokens<'a> {
    type Target = [SToken<'a>];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> Tokens<'a> {
    pub fn new(tok: &'a [SToken]) -> Self {
        Tokens(tok)
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens(&self.0[0..count])
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (Tokens(suffix), Tokens(prefix))
    }
}

macro_rules! impl_slice {
    ($ty:ty) => {
        impl<'a> Slice<$ty> for Tokens<'a> {
            #[inline]
            fn slice(&self, range: $ty) -> Self {
                Tokens(self.0.slice(range))
            }
        }
    };
}

impl_slice!(Range<usize>);
impl_slice!(RangeTo<usize>);
impl_slice!(RangeFrom<usize>);
impl_slice!(RangeFull);

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a SToken<'a>;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = ::std::slice::Iter<'a, SToken<'a>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.0.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.0.len()))
        }
    }
}

impl<'a, 'b> Compare<Token> for Tokens<'a> {
    fn compare(&self, t: Token) -> CompareResult {
        if self.0.is_empty() {
            CompareResult::Incomplete
        } else if self.0[0].value == t {
            CompareResult::Ok
        } else {
            CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: Token) -> CompareResult {
        if self.0.is_empty() {
            return CompareResult::Incomplete;
        }
        match (self.0[0].value, t) {
            (Token::Integer(_), Token::Integer(_)) => CompareResult::Ok,
            _ => self.compare(t),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Node {
    Integer(i64),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
}

fn parse_parens(input: Tokens) -> IResult<Tokens, Node> {
    delimited(tag(Token::LParen), parse_expr, tag(Token::RParen))(input)
}

fn parse_integer(input: Tokens) -> IResult<Tokens, Node> {
    let (i1, t) = take(1usize)(input)?;
    match t[0].value {
        Token::Integer(i) => Ok((i1, Node::Integer(i))),
        _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn parse_primary(input: Tokens) -> IResult<Tokens, Node> {
    alt((parse_parens, parse_integer))(input)
}

fn fold_exprs(init: Node, remainder: Vec<(Token, Node)>) -> Node {
    remainder
        .into_iter()
        .fold(init, |acc, (token, expr)| match token {
            Token::Plus => Node::Add(Box::new(acc), Box::new(expr)),
            Token::Minus => Node::Sub(Box::new(acc), Box::new(expr)),
            Token::Multiply => Node::Mul(Box::new(acc), Box::new(expr)),
            Token::Divide => Node::Div(Box::new(acc), Box::new(expr)),
            _ => panic!("unexpected token"),
        })
}

fn parse_infix<'a, E, F, G>(
    first: F,
    second: G,
) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Node, E>
where
    F: Parser<Tokens<'a>, Tokens<'a>, E>,
    G: Parser<Tokens<'a>, Node, E> + Copy,
    E: ParseError<Tokens<'a>>,
{
    map(
        pair(second, many0(pair(map(first, |x| *x[0]), second))),
        |(i, r)| fold_exprs(i, r),
    )
}

fn parse_mul(input: Tokens) -> IResult<Tokens, Node> {
    parse_infix(
        alt((tag(Token::Multiply), tag(Token::Divide))),
        parse_primary,
    )(input)
}

fn parse_add(input: Tokens) -> IResult<Tokens, Node> {
    parse_infix(alt((tag(Token::Plus), tag(Token::Minus))), parse_mul)(input)
}

fn parse_expr(input: Tokens) -> IResult<Tokens, Node> {
    parse_add(input)
}

fn parse_tokens(input: Tokens) -> IResult<Tokens, Node> {
    terminated(parse_expr, tag(Token::Eof))(input)
}

fn push() {
    println!("  push %rax");
}

fn pop(r: &str) {
    println!("  pop {}", r);
}

fn gen_expr(expr: Node) {
    match expr {
        Node::Integer(i) => {
            println!("  mov ${}, %rax", i)
        }
        Node::Add(lhs, rhs) => {
            gen_expr(*rhs);
            push();
            gen_expr(*lhs);
            pop("%rdi");
            println!("  add %rdi, %rax");
        }
        Node::Sub(lhs, rhs) => {
            gen_expr(*rhs);
            push();
            gen_expr(*lhs);
            pop("%rdi");
            println!("  sub %rdi, %rax");
        }
        Node::Mul(lhs, rhs) => {
            gen_expr(*rhs);
            push();
            gen_expr(*lhs);
            pop("%rdi");
            println!("  imul %rdi, %rax");
        }
        Node::Div(lhs, rhs) => {
            gen_expr(*rhs);
            push();
            gen_expr(*lhs);
            pop("%rdi");
            println!("  cqo");
            println!("  idiv %rdi");
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("invalid number of arguments")
    }

    let input = Span::new(&args[1]);
    let x = lex(input).finish();
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
    let (_, result) = parse_tokens(tokens).unwrap();

    println!("  .globl main");
    println!("main:");

    gen_expr(result);

    println!("  ret");

    Ok(())
}
