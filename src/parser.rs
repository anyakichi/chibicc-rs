use std::iter::Enumerate;
use std::ops::{Deref, Range, RangeFrom, RangeFull, RangeTo};

use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, opt};
use nom::error::{Error, ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{
    Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake, Needed, Parser, Slice,
};

use crate::lexer::{SToken, Token};

#[derive(Clone, Copy, Debug)]
pub struct Tokens<'a>(&'a [SToken<'a>]);

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
        match (&self.0[0].value, &t) {
            (Token::Integer(_), Token::Integer(_)) => CompareResult::Ok,
            _ => self.compare(t),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Assign(Box<Node>, Box<Node>),
    Var(String),
    Integer(i64),
    Neg(Box<Node>),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Eq(Box<Node>, Box<Node>),
    Ne(Box<Node>, Box<Node>),
    Le(Box<Node>, Box<Node>),
    Lt(Box<Node>, Box<Node>),
    Return(Box<Node>),
    Block(Vec<Node>),
}

fn parens(input: Tokens) -> IResult<Tokens, Node> {
    delimited(tag(Token::LParen), expr, tag(Token::RParen))(input)
}

fn r#return(input: Tokens) -> IResult<Tokens, Node> {
    preceded(tag(Token::Return), map(expr, |x| Node::Return(Box::new(x))))(input)
}

fn identifier(input: Tokens) -> IResult<Tokens, Node> {
    let (i1, t) = take(1usize)(input)?;
    match &t[0].value {
        Token::Ident(i) => Ok((i1, Node::Var(i.to_string()))),
        _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn integer(input: Tokens) -> IResult<Tokens, Node> {
    let (i1, t) = take(1usize)(input)?;
    match t[0].value {
        Token::Integer(i) => Ok((i1, Node::Integer(i))),
        _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn primary(input: Tokens) -> IResult<Tokens, Node> {
    alt((parens, identifier, integer))(input)
}

fn unary(input: Tokens) -> IResult<Tokens, Node> {
    alt((
        preceded(tag(Token::Plus), unary),
        map(preceded(tag(Token::Minus), unary), |x| {
            Node::Neg(Box::new(x))
        }),
        primary,
    ))(input)
}

fn infix_expr(token: &Token, lhs: Node, rhs: Node) -> Node {
    match token {
        Token::Plus => Node::Add(Box::new(lhs), Box::new(rhs)),
        Token::Minus => Node::Sub(Box::new(lhs), Box::new(rhs)),
        Token::Multiply => Node::Mul(Box::new(lhs), Box::new(rhs)),
        Token::Divide => Node::Div(Box::new(lhs), Box::new(rhs)),
        Token::GreaterThanEqual => Node::Le(Box::new(rhs), Box::new(lhs)),
        Token::GreaterThan => Node::Lt(Box::new(rhs), Box::new(lhs)),
        Token::LessThanEqual => Node::Le(Box::new(lhs), Box::new(rhs)),
        Token::LessThan => Node::Lt(Box::new(lhs), Box::new(rhs)),
        Token::Equal => Node::Eq(Box::new(lhs), Box::new(rhs)),
        Token::NotEqual => Node::Ne(Box::new(lhs), Box::new(rhs)),
        Token::Assign => Node::Assign(Box::new(lhs), Box::new(rhs)),
        _ => panic!("unexpected token"),
    }
}

fn fold_infix<'a, E, F, G>(
    parser: F,
    op: G,
) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Node, E>
where
    F: Parser<Tokens<'a>, Node, E> + Copy,
    G: Parser<Tokens<'a>, Tokens<'a>, E>,
    E: ParseError<Tokens<'a>>,
{
    map(pair(parser, many0(pair(op, parser))), |(i, xs)| {
        xs.into_iter().fold(i, |l, (o, r)| infix_expr(&*o[0], l, r))
    })
}

fn infix<'a, E, F, G, H>(
    left: F,
    op: G,
    right: H,
) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Node, E>
where
    F: Parser<Tokens<'a>, Node, E>,
    G: Parser<Tokens<'a>, Tokens<'a>, E>,
    H: Parser<Tokens<'a>, Node, E>,
    E: ParseError<Tokens<'a>>,
{
    map(pair(left, opt(pair(op, right))), |(l, opt)| match opt {
        Some((o, r)) => infix_expr(&*o[0], l, r),
        None => l,
    })
}

fn mul(input: Tokens) -> IResult<Tokens, Node> {
    fold_infix(unary, alt((tag(Token::Multiply), tag(Token::Divide))))(input)
}

fn add(input: Tokens) -> IResult<Tokens, Node> {
    fold_infix(mul, alt((tag(Token::Plus), tag(Token::Minus))))(input)
}

fn relational(input: Tokens) -> IResult<Tokens, Node> {
    fold_infix(
        add,
        alt((
            tag(Token::GreaterThan),
            tag(Token::GreaterThanEqual),
            tag(Token::LessThan),
            tag(Token::LessThanEqual),
        )),
    )(input)
}

fn equality(input: Tokens) -> IResult<Tokens, Node> {
    fold_infix(relational, alt((tag(Token::Equal), tag(Token::NotEqual))))(input)
}

fn assign(input: Tokens) -> IResult<Tokens, Node> {
    infix(equality, tag(Token::Assign), assign)(input)
}

fn expr(input: Tokens) -> IResult<Tokens, Node> {
    assign(input)
}

fn stmt(input: Tokens) -> IResult<Tokens, Node> {
    terminated(alt((r#return, expr)), tag(Token::SemiColon))(input)
}

fn block(input: Tokens) -> IResult<Tokens, Node> {
    map(
        delimited(
            tag(Token::LBrace),
            many0(alt((block, stmt))),
            tag(Token::RBrace),
        ),
        Node::Block,
    )(input)
}

pub fn parse(input: Tokens) -> IResult<Tokens, Node> {
    terminated(block, tag(Token::Eof))(input)
}
