use miette::{Diagnostic, SourceSpan};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, newline},
    combinator::map,
};
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    whole: &'a str,
    rest: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            whole: code,
            rest: code,
            position: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rest.len() == 0 {
            return None;
        }

        // `alt` only supports up to 21 tuple values, hence they have been split up
        let result: nom::IResult<&str, Token, nom::error::Error<&str>> = alt((
            // Keyword
            alt((
                map(tag("and"), |_| Token::And),
                map(tag("as"), |_| Token::As),
                map(tag("assert"), |_| Token::Assert),
                map(tag("break"), |_| Token::Break),
                map(tag("class"), |_| Token::Class),
                map(tag("continue"), |_| Token::Continue),
                map(tag("def"), |_| Token::Def),
                map(tag("del"), |_| Token::Del),
                map(tag("elif"), |_| Token::Elif),
                map(tag("else"), |_| Token::Else),
                map(tag("except"), |_| Token::Except),
                map(tag("false"), |_| Token::False),
                map(tag("finally"), |_| Token::Finally),
                map(tag("for"), |_| Token::For),
                map(tag("from"), |_| Token::From),
                map(tag("global"), |_| Token::Global),
                map(tag("if"), |_| Token::If),
            )),
            alt((
                map(tag("import"), |_| Token::Import),
                map(tag("in"), |_| Token::In),
                map(tag("is"), |_| Token::Is),
                map(tag("lambda"), |_| Token::Lambda),
                map(tag("None"), |_| Token::None),
                map(tag("nonlocal"), |_| Token::Nonlocal),
                map(tag("not"), |_| Token::Not),
                map(tag("or"), |_| Token::Or),
                map(tag("pass"), |_| Token::Pass),
                map(tag("raise"), |_| Token::Raise),
                map(tag("return"), |_| Token::Return),
                map(tag("True"), |_| Token::True),
                map(tag("try"), |_| Token::Try),
                map(tag("while"), |_| Token::While),
                map(tag("with"), |_| Token::With),
                map(tag("yield"), |_| Token::Yield),
            )),
            // Triple char
            alt((
                map(tag(">>="), |_| Token::DoubleGreaterThanEquals),
                map(tag("<<="), |_| Token::DoubleLessThanEquals),
                map(tag("//="), |_| Token::DoubleSlashEquals),
                map(tag("**="), |_| Token::DoubleStarEquals),
            )),
            // Double char
            alt((
                map(tag("&="), |_| Token::AmpersandEquals),
                map(tag("|="), |_| Token::BarEquals),
                map(tag("^|"), |_| Token::CaratEquals),
                map(tag(":="), |_| Token::ColonEquals),
                map(tag(">>"), |_| Token::DoubleGreaterThan),
                map(tag("<<"), |_| Token::DoubleLessThan),
                map(tag("//"), |_| Token::DoubleSlash),
                map(tag("**"), |_| Token::DoubleStar),
                map(tag("-="), |_| Token::MinusEquals),
                map(tag("%="), |_| Token::PercentEquals),
                map(tag("+="), |_| Token::PlusEquals),
                map(tag("/="), |_| Token::SlashEquals),
                map(tag("*="), |_| Token::StarEquals),
            )),
            // Single char
            alt((
                map(char('&'), |_| Token::Ampersand),
                map(char('\\'), |_| Token::BackSlash),
                map(char('|'), |_| Token::Bar),
                map(char('^'), |_| Token::Carat),
                map(char(':'), |_| Token::Colon),
                map(char(','), |_| Token::Comma),
                map(char('.'), |_| Token::Dot),
                map(char('='), |_| Token::Equals),
                map(char('>'), |_| Token::GreaterThan),
                map(char('{'), |_| Token::LeftBrace),
                map(char('['), |_| Token::LeftBracket),
                map(char('('), |_| Token::LeftParen),
            )),
            alt((
                map(char('<'), |_| Token::LessThan),
                map(newline, |_| Token::NewLine),
                map(char('-'), |_| Token::Minus),
                map(char('%'), |_| Token::Percent),
                map(char('+'), |_| Token::Plus),
                map(char('}'), |_| Token::RightBrace),
                map(char(']'), |_| Token::RightBracket),
                map(char(')'), |_| Token::RightParen),
                map(char('/'), |_| Token::Slash),
                map(char('*'), |_| Token::Star),
                map(char('~'), |_| Token::Tilde),
            )),
        ))(self.rest);
        match result {
            Ok((rest, token)) => {
                // Update position based on how much the length of `rest` decreased
                self.position += self.rest.len() - rest.len();
                self.rest = rest;
                Some(Ok(token))
            }
            Err(_) => {
                let at = self.position;
                let line = self.whole[0..=self.position].lines().count();
                let column = at
                    - self.whole[..self.position]
                        .rfind('\n')
                        .map(|i| i + 1)
                        .unwrap_or(0)
                    + 1;
                Some(Err(LexError {
                    src: self.whole.to_owned(),
                    at: at.into(),
                    line,
                    column,
                }))
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Ampersand,
    AmpersandEquals,
    And,
    As,
    Assert,
    BackSlash,
    Bar,
    BarEquals,
    Break,
    Carat,
    CaratEquals,
    Class,
    Colon,
    ColonEquals,
    Comma,
    Complex(&'a str),
    Continue,
    Def,
    Del,
    Dot,
    DoubleGreaterThan,
    DoubleGreaterThanEquals,
    DoubleLessThan,
    DoubleLessThanEquals,
    DoubleSlash,
    DoubleSlashEquals,
    DoubleStar,
    DoubleStarEquals,
    Elif,
    Else,
    Equals,
    Except,
    False,
    Finally,
    Float(&'a str),
    For,
    From,
    Global,
    GreaterThan,
    If,
    Import,
    In,
    Int(&'a str),
    Is,
    Lambda,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    Minus,
    MinusEquals,
    NewLine,
    None,
    Nonlocal,
    Not,
    Or,
    Pass,
    Percent,
    PercentEquals,
    Plus,
    PlusEquals,
    Raise,
    Return,
    RightBrace,
    RightBracket,
    RightParen,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    Tilde,
    True,
    Try,
    While,
    With,
    Yield,
}

#[derive(Debug, Diagnostic, Error)]
#[error("unrecognized token on line {line}, column {column}")]
#[diagnostic(code("lexing error"))]
pub struct LexError {
    #[source_code]
    pub src: String,

    #[label("here")]
    pub at: SourceSpan,
    pub line: usize,
    pub column: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ensure_taking_most_chars_possible() {
        let mut lexer = Lexer::new(r"***==*None\/");
        assert_eq!(lexer.next().unwrap().unwrap(), Token::DoubleStar);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::StarEquals);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Equals);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Star);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::None);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::BackSlash);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Slash);
        assert!(lexer.next().is_none());
    }

    #[test]
    fn lex_error_location() {
        let mut lexer = Lexer::new("*/foo");
        for _ in 0..2 {
            lexer.next();
        }
        let error = lexer.next().unwrap().unwrap_err();
        assert_eq!(error.at, SourceSpan::from(2));
        assert_eq!(error.line, 1);
        assert_eq!(error.column, 3);

        let mut lexer = Lexer::new("*/\nfoo");
        for _ in 0..3 {
            lexer.next();
        }
        let error = lexer.next().unwrap().unwrap_err();
        assert_eq!(error.at, SourceSpan::from(3));
        assert_eq!(error.line, 2);
        assert_eq!(error.column, 1);

        let mut lexer = Lexer::new("*/\n*foo");
        for _ in 0..4 {
            lexer.next();
        }
        let error = lexer.next().unwrap().unwrap_err();
        assert_eq!(error.at, SourceSpan::from(4));
        assert_eq!(error.line, 2);
        assert_eq!(error.column, 2);
    }

    #[test]
    fn numbers() {
        let mut lexer = Lexer::new("100-1_0_0_234-1.1--5.2+.6++.5*83.2_02-1j/3+5J-10_0j+0b1_00-0B10_1+0o80_2-0O2+0xFaB4/0XFF_A_D");
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("100"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("1_0_0_234"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Float("1.1"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Float("-5.2"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Float("+.6"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Plus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Float("+.5"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Star);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Complex("83.2_02-1j"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Slash);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Complex("3+5J"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Complex("-10_0j"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Plus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("0b1_00"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("0B10_1"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Plus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("0o80_2"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Minus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("0O2"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Plus);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("0xFaB4"));
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Slash);
        assert_eq!(lexer.next().unwrap().unwrap(), Token::Int("0xFF_A_D"));
        assert!(lexer.next().is_none());
    }
}
