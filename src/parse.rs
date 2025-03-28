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

    pub fn parse(&mut self) -> Result<Option<Statement>, ParseError> {
        todo!()
    }

    fn parse_expression(&mut self) -> Result<Option<Statement>, ParseError> {
        todo!()
    }

    fn parse_expression_bp(&mut self) -> Result<Option<Statement>, ParseError> {
        todo!()
    }
}

pub enum Operand {
    Ampersand,
    AmpersandEquals,
    And,
    As,
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
    GreaterThanEquals,
    In,
    Is,
    LessThan,
    LessThanEquals,
    Minus,
    MinusEquals,
    Not,
    NotEquals,
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

impl Operand {
    fn prefix_binding_power(&self) -> Option<u8> {
        use Operand::*;
        match *self {
            Not => Some(16),
            Tilde => Some(15),
            Minus | Plus => Some(14),
            _ => None,
        }
    }

    fn infix_binding_power(&self) -> (u8, u8) {
        use Operand::*;
        match *self {
            Dot => (17, 17),
            Not => (16, 16),
            DoubleStar => (15, 14),
            Tilde => (15, 15),
            DoubleSlash | Percent | Slash | Star => (13, 13),
            Minus | Plus => (12, 12),
            DoubleGreaterThan | DoubleLessThan => (11, 11),
            Ampersand => (10, 10),
            Carat => (9, 9),
            Bar => (8, 8),
            In | Is => (7, 7),
            As | GreaterThan | GreaterThanEquals | LessThan | LessThanEquals | NotEquals => (6, 6),
            ColonEquals => (5, 4),
            And => (4, 5),
            Or => (3, 6),
            AmpersandEquals
            | BarEquals
            | CaratEquals
            | DoubleGreaterThanEquals
            | DoubleLessThanEquals
            | DoubleSlashEquals
            | DoubleStarEquals
            | Equals
            | MinusEquals
            | PercentEquals
            | PlusEquals
            | SlashEquals
            | StarEquals => (2, 1),
            Comma => (1, 1),
            Colon => (0, 0),
        }
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
    Boolean(bool),
    Lambda {
        arguments: Vec<String>,
        body: Box<Expression>,
    },
    Literal(String),
    None,
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
