use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::lex::Token;
use std::iter::Peekable;

pub struct Parser<'a, I: Iterator<Item = &'a Token<'a>>> {
    tokens: Peekable<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = &'a Token<'a>>,
{
    pub fn new(tokens: Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Result<Option<Ast<'_>>, ParseError> {
        todo!()
    }

    fn parse_expression(&mut self) -> Result<Option<Ast<'_>>, ParseError> {
        todo!()
    }

    fn parse_expression_bp(&mut self) -> Result<Option<Ast<'_>>, ParseError> {
        todo!()
    }
}

pub enum Ast<'a> {
    Atom(Atom<'a>),
    Cons(Operand, Vec<Ast<'a>>),
}

pub enum Atom<'a> {
    Complex(&'a str),
    False,
    Float(&'a str),
    Int(&'a str),
    None,
    String(&'a str),
    True,
}

pub enum Operand {
    Ampersand,
    AmpersandEquals,
    And,
    As,
    Assert,
    Bar,
    BarEquals,
    Carat,
    CaratEquals,
    Colon,
    ColonEquals,
    Comma,
    Dot,
    DoubleGreaterThan,
    DoubleGreaterThanEquals,
    DoubleLessThan,
    DoubleLessThanEquals,
    DoubleSlash,
    DoubleSlashEquals,
    DoubleStar,
    DoubleStarEquals,
    Equals,
    GreaterThan,
    In,
    Is,
    LessThan,
    Minus,
    MinusEquals,
    Not,
    Or,
    Percent,
    PercentEquals,
    Plus,
    PlusEquals,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    Tilde,
}

fn infix_binding_power(operand: &Operand) -> (u8, u8) {
    use Operand::*;
    match operand {
        Ampersand => todo!(),
        AmpersandEquals => todo!(),
        And => todo!(),
        As => todo!(),
        Assert => todo!(),
        Bar => todo!(),
        BarEquals => todo!(),
        Carat => todo!(),
        CaratEquals => todo!(),
        Colon => todo!(),
        ColonEquals => todo!(),
        Comma => todo!(),
        Dot => todo!(),
        DoubleGreaterThan => todo!(),
        DoubleGreaterThanEquals => todo!(),
        DoubleLessThan => todo!(),
        DoubleLessThanEquals => todo!(),
        DoubleSlash => todo!(),
        DoubleSlashEquals => todo!(),
        DoubleStar => todo!(),
        DoubleStarEquals => todo!(),
        Equals => todo!(),
        GreaterThan => todo!(),
        In => todo!(),
        Is => todo!(),
        LessThan => todo!(),
        Minus => todo!(),
        MinusEquals => todo!(),
        Not => todo!(),
        Or => todo!(),
        Percent => todo!(),
        PercentEquals => todo!(),
        Plus => todo!(),
        PlusEquals => todo!(),
        Slash => todo!(),
        SlashEquals => todo!(),
        Star => todo!(),
        StarEquals => todo!(),
        Tilde => todo!(),
    }
}

pub enum Number {
    Float(f64),
    Int(isize),
    Complex {
        real: Box<Number>,
        imaginary: Box<Number>,
    },
}

pub enum Expression {
    Lambda {
        arguments: Vec<String>,
        body: Box<Expression>,
    },
    Literal(String),
    Number(Number),
    Operand {
        operand: Operand,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

pub enum Statement {
    Block(Vec<Statement>),
    Break,
    Class {
        name: String,
        body: Vec<Statement>,
    },
    Continue,
    Def {
        name: String,
        body: Box<Statement>,
    },
    Del(Expression),
    Expression(Expression),
    For {
        init: Expression,
        condition: Expression,
        update: Box<Statement>,
    },
    Global(String),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Import {
        module: Expression,
        from: Option<Expression>,
        import_as: Option<Expression>,
    },
    Nonlocal(String),
    Pass,
    Raise(Expression),
    Return(Option<Expression>),
    Try {
        body: Box<Statement>,
        except: Vec<(Expression, Box<Statement>)>,
        finally: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    With {
        using: Expression,
        new_name: Option<String>,
    },
    Yield(Option<Expression>),
}

#[derive(Debug, Diagnostic, Error)]
pub enum ParseError {
    #[error("invalid syntax at line {line}, column {column}")]
    InvalidSyntax {
        #[source_code]
        src: String,

        #[label("here")]
        at: SourceSpan,

        line: usize,
        column: usize,
    },
}
