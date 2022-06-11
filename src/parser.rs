use std::iter::Enumerate;
use std::ops::{Deref, Range, RangeFrom, RangeFull, RangeTo};

use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, opt, value};
use nom::error::{Error, ErrorKind, ParseError};
use nom::multi::{fold_many0, many0, many0_count, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
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

pub type Program = Vec<Definition>;

#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
    Declaration(Declaration),
    Function {
        typ: Type,
        name: String,
        params: Vec<Declaration>,
        body: Box<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub typ: Type,
    pub name: String,
    pub init: Option<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),
    Decl(Vec<Declaration>),
    Expr(Node),
    Return(Node),
    If {
        cond: Node,
        then: Box<Statement>,
        r#else: Option<Box<Statement>>,
    },
    Iter {
        init: Option<Node>,
        cond: Option<Node>,
        next: Option<Node>,
        then: Box<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Assign(Box<Node>, Box<Node>),
    Var(String),
    Integer(i64),
    Neg(Box<Node>),
    Addr(Box<Node>),
    Deref(Box<Node>),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Eq(Box<Node>, Box<Node>),
    Ne(Box<Node>, Box<Node>),
    Le(Box<Node>, Box<Node>),
    Lt(Box<Node>, Box<Node>),
    Call(String, Vec<Node>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Function(Box<Type>, Vec<Type>),
    Int,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Pointer(_) => 8,
            Type::Array(t, l) => t.size() * l,
            Type::Function(_, _) => 1,
            Type::Int => 8,
        }
    }
}

fn braces<'a, E, F, O>(parser: F) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, O, E>
where
    F: Parser<Tokens<'a>, O, E>,
    E: ParseError<Tokens<'a>>,
{
    delimited(tag(Token::LBrace), parser, tag(Token::RBrace))
}

fn brackets<'a, E, F, O>(parser: F) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, O, E>
where
    F: Parser<Tokens<'a>, O, E>,
    E: ParseError<Tokens<'a>>,
{
    delimited(tag(Token::LBracket), parser, tag(Token::RBracket))
}

fn parens<'a, E, F, O>(parser: F) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, O, E>
where
    F: Parser<Tokens<'a>, O, E>,
    E: ParseError<Tokens<'a>>,
{
    delimited(tag(Token::LParen), parser, tag(Token::RParen))
}

fn r#return(input: Tokens) -> IResult<Tokens, Statement> {
    preceded(tag(Token::Return), map(expr, Statement::Return))(input)
}

fn ident(input: Tokens) -> IResult<Tokens, String> {
    let (i1, t) = take(1usize)(input)?;
    match &t[0].value {
        Token::Ident(i) => Ok((i1, i.to_string())),
        _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn identifier(input: Tokens) -> IResult<Tokens, Node> {
    map(ident, Node::Var)(input)
}

fn integer(input: Tokens) -> IResult<Tokens, Node> {
    let (i1, t) = take(1usize)(input)?;
    match t[0].value {
        Token::Integer(i) => Ok((i1, Node::Integer(i))),
        _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn primary(input: Tokens) -> IResult<Tokens, Node> {
    alt((call, identifier, integer, parens(expr)))(input)
}

fn call(input: Tokens) -> IResult<Tokens, Node> {
    map(
        tuple((ident, parens(separated_list0(tag(Token::Comma), expr)))),
        |(name, params)| Node::Call(name, params),
    )(input)
}

fn postfix(input: Tokens) -> IResult<Tokens, Node> {
    fn parser(input: Tokens) -> IResult<Tokens, impl FnOnce(Node) -> Node> {
        map(brackets(expr), |e: Node| {
            |x: Node| Node::Deref(Box::new(Node::Add(Box::new(x), Box::new(e))))
        })(input)
    }

    let (input, init) = primary(input)?;
    fold_many0(parser, move || init.clone(), |acc, f| f(acc))(input)
}

fn unary(input: Tokens) -> IResult<Tokens, Node> {
    alt((
        preceded(tag(Token::Plus), unary),
        map(preceded(tag(Token::Minus), unary), |x| {
            Node::Neg(Box::new(x))
        }),
        map(preceded(tag(Token::And), unary), |x| {
            Node::Addr(Box::new(x))
        }),
        map(preceded(tag(Token::Multiply), unary), |x| {
            Node::Deref(Box::new(x))
        }),
        postfix,
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
    F: Parser<Tokens<'a>, Node, E> + Copy,
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

fn declspec(input: Tokens) -> IResult<Tokens, Type> {
    value(Type::Int, tag(Token::Int))(input)
}

fn declarator<'a>(typ: Type) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Declaration> {
    move |input: Tokens<'a>| {
        map(
            tuple((
                many0_count(tag(Token::Multiply)),
                ident,
                many0(brackets(integer)),
            )),
            |(n, name, suffix)| {
                let typ = (0..n).fold(typ.clone(), |a, _| Type::Pointer(Box::new(a)));
                let typ = suffix.iter().rev().fold(typ, |a, x| match x {
                    Node::Integer(i) => Type::Array(Box::new(a), *i as usize),
                    _ => panic!("unexpected array length"),
                });
                Declaration {
                    name,
                    typ,
                    init: None,
                }
            },
        )(input)
    }
}

fn init_declarator<'a>(typ: Type) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Declaration> {
    move |input: Tokens<'a>| {
        map(
            pair(
                declarator(typ.clone()),
                opt(preceded(tag(Token::Assign), expr)),
            ),
            |(decl, init)| Declaration {
                init: init
                    .map(|x| Node::Assign(Box::new(Node::Var(decl.name.clone())), Box::new(x))),
                ..decl
            },
        )(input)
    }
}

fn declaration(input: Tokens) -> IResult<Tokens, Statement> {
    let (input, typ) = declspec(input)?;
    let (input, decls) = terminated(
        separated_list1(tag(Token::Comma), init_declarator(typ)),
        tag(Token::SemiColon),
    )(input)?;
    Ok((input, Statement::Decl(decls)))
}

fn param(input: Tokens) -> IResult<Tokens, Declaration> {
    let (input, typ) = declspec(input)?;
    declarator(typ)(input)
}

fn if_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            tag(Token::If),
            parens(expr),
            map(stmt, Box::new),
            opt(map(preceded(tag(Token::Else), stmt), Box::new)),
        )),
        |(_, cond, then, r#else)| Statement::If { cond, then, r#else },
    )(input)
}

fn for_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            tag(Token::For),
            tag(Token::LParen),
            opt(expr),
            tag(Token::SemiColon),
            opt(expr),
            tag(Token::SemiColon),
            opt(expr),
            tag(Token::RParen),
            map(stmt, Box::new),
        )),
        |(_, _, init, _, cond, _, next, _, then)| Statement::Iter {
            init,
            cond,
            next,
            then,
        },
    )(input)
}

fn while_stmt(input: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            tag(Token::While),
            map(parens(expr), Some),
            map(stmt, Box::new),
        )),
        |(_, cond, then)| Statement::Iter {
            init: None,
            cond,
            next: None,
            then,
        },
    )(input)
}

fn block(input: Tokens) -> IResult<Tokens, Statement> {
    map(braces(many0(alt((stmt, declaration)))), Statement::Block)(input)
}

fn stmt(input: Tokens) -> IResult<Tokens, Statement> {
    alt((
        if_stmt,
        for_stmt,
        while_stmt,
        map(terminated(expr, tag(Token::SemiColon)), Statement::Expr),
        terminated(r#return, tag(Token::SemiColon)),
        value(Statement::Block(vec![]), tag(Token::SemiColon)),
        block,
    ))(input)
}

fn function(input: Tokens) -> IResult<Tokens, Definition> {
    let (input, typ) = declspec(input)?;
    map(
        tuple((
            declarator(typ),
            parens(separated_list0(tag(Token::Comma), param)),
            map(block, Box::new),
        )),
        |(f, params, body)| Definition::Function {
            typ: Type::Function(
                Box::new(f.typ),
                params.iter().map(|x| x.typ.clone()).collect(),
            ),
            name: f.name,
            params,
            body,
        },
    )(input)
}

pub fn parse(input: Tokens) -> IResult<Tokens, Vec<Definition>> {
    terminated(many0(function), tag(Token::Eof))(input)
}
