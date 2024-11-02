use miette::{miette, IntoDiagnostic, Result};
use nom::{branch::alt, bytes::complete::tag, character::complete::char, combinator::map};

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    rest: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self { rest: code }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token>;

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
            )),
            alt((
                map(char('('), |_| Token::LeftParen),
                map(char('<'), |_| Token::LessThan),
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
                self.rest = rest;
                Some(Ok(token))
            }
            Err(nom::Err::Error(e) | nom::Err::Failure(e)) => Some(Err(miette!(e.to_string()))),
            Err(_) => Some(Err(miette!("b"))),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
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
    For,
    From,
    Global,
    GreaterThan,
    If,
    Import,
    In,
    Is,
    Lambda,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    Minus,
    MinusEquals,
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
    fn foo() {
        let mut lexer = Lexer::new("foo");
        dbg!(lexer.next().unwrap());
        panic!();
    }
}
