use miette::{Diagnostic, SourceSpan};
use nom::{
    branch::alt,
    bytes::complete::{is_a, tag, tag_no_case, take_till, take_until, take_while},
    character::complete::{alpha1, char, digit1, line_ending, newline, one_of, space0, space1},
    combinator::{cut, eof, map, opt, peek, recognize, verify},
    multi::{fold_many0, many0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use std::collections::VecDeque;
use thiserror::Error;

static COLUMN_WIDTH: u8 = 8;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    whole: &'a str,
    rest: &'a str,
    position: usize,
    indent_stack: Vec<IndentationLevel>,
    new_line: bool,
    token_buffer: VecDeque<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            whole: code,
            rest: code,
            position: 0,
            indent_stack: Vec::new(),
            new_line: true,
            token_buffer: VecDeque::new(),
        }
    }

    fn parse_number(input: &str) -> IResult<&str, Token> {
        alt((Self::parse_complex, Self::parse_float, Self::parse_int))(input)
    }

    fn parse_complex(input: &str) -> IResult<&str, Token> {
        map(
            recognize(pair(
                opt(pair(
                    alt((Self::parse_float, Self::parse_int)),
                    one_of("+-"),
                )),
                pair(alt((Self::parse_float, Self::parse_decimal)), one_of("jJ")),
            )),
            |s| Token::Complex(s),
        )(input)
    }
    fn parse_float(input: &str) -> IResult<&str, Token> {
        map(
            verify(
                recognize(tuple((
                    opt(Self::parse_decimal),
                    opt(char('.')),
                    opt(Self::parse_decimal),
                    opt(tuple((
                        one_of("eE"),
                        opt(one_of("+-")),
                        Self::parse_decimal,
                    ))),
                ))),
                |s: &str| s != "." && (s.contains('.') || s.contains('e') || s.contains('E')),
            ),
            |s| Token::Float(s),
        )(input)
    }

    fn parse_int(input: &str) -> IResult<&str, Token> {
        alt((
            map(
                recognize(alt((
                    preceded(
                        tag_no_case("0b"),
                        pair(is_a("01"), many0(pair(opt(char('_')), is_a("01")))),
                    ),
                    preceded(
                        tag_no_case("0o"),
                        pair(
                            is_a("01234567"),
                            many0(pair(opt(char('_')), is_a("01234567"))),
                        ),
                    ),
                    preceded(
                        tag_no_case("0x"),
                        pair(
                            is_a("0123456789abcdefABCDEF"),
                            many0(pair(opt(char('_')), is_a("0123456789abcdefABCDEF"))),
                        ),
                    ),
                ))),
                |s| Token::Int(s),
            ),
            Self::parse_decimal,
        ))(input)
    }

    fn parse_decimal(input: &str) -> IResult<&str, Token> {
        map(
            recognize(pair(digit1, many0(pair(opt(char('_')), digit1)))),
            |s| Token::Int(s),
        )(input)
    }

    fn parse_string(input: &str) -> IResult<&str, Token> {
        map(
            recognize(pair(
                opt(alt((
                    tag_no_case("rf"),
                    tag_no_case("fr"),
                    tag_no_case("rb"),
                    tag_no_case("br"),
                    tag_no_case("r"),
                    tag_no_case("f"),
                    tag_no_case("b"),
                    tag_no_case("u"),
                ))),
                alt((
                    delimited(tag("\"\"\""), cut(take_until("\"\"\"")), tag("\"\"\"")),
                    delimited(tag("'''"), cut(take_until("'''")), tag("'''")),
                    delimited(
                        char('\''),
                        cut(take_till(|c| matches!(c, '\'' | '\r' | '\n'))),
                        char('\''),
                    ),
                    delimited(
                        char('"'),
                        cut(take_till(|c| matches!(c, '"' | '\r' | '\n'))),
                        char('"'),
                    ),
                )),
            )),
            |s| Token::String(s),
        )(input)
    }

    fn parse_comment(input: &str) -> IResult<&str, ()> {
        // TODO: Parse triple-quoted docstrings
        map(tuple((space0, char('#'), take_till(|c| c == '\n'))), |_| ())(input)
    }

    fn parse_identifier(input: &str) -> IResult<&str, Token> {
        map(
            recognize(pair(
                alt((tag("_"), alpha1)),
                opt(take_while(|c: char| c.is_alphanumeric() || c == '_')),
            )),
            |s| Token::Identifier(s),
        )(input)
    }

    // We are following the rules used by the CPython lexer here. In summary:
    //
    // 1. A tab advances the cursor up to the next 8 column boundary.
    // 2. The line is more indented if the column that the spaces and tabs result in is
    //    greater than the current indentation level.
    // 3. If the column value is the same as one already on the stack, but the number of
    //    spaces and tabs differs, this is a error.
    fn parse_indent(input: &str) -> IResult<&str, IndentationLevel> {
        fold_many0(
            one_of(" \t"),
            || IndentationLevel::default(),
            |mut acc, item| {
                match item {
                    ' ' => {
                        acc.spaces += 1;
                        acc.column += 1;
                    }
                    '\t' => {
                        acc.tabs += 1;
                        // https://github.com/python/cpython/blob/ce79274e9f093bd06d2285c9af48dbcbc92173de/Parser/lexer/lexer.c#L430
                        acc.column = (acc.column / COLUMN_WIDTH + 1) * COLUMN_WIDTH;
                    }
                    _ => unreachable!(),
                }
                acc
            },
        )(input)
    }

    fn parse_empty_line(input: &str) -> IResult<&str, ()> {
        map(pair(space0, peek(newline)), |_| ())(input)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: Ignore newline if there are pending unclosed brackets or line ended with '\'
        // TODO: When encountering '\', the indentation we have seen until it is the indentation for
        //       the next line
        // TODO: Tests for indentation
        dbg!(rest);
        if self.token_buffer.len() > 0 {
            return Some(Ok(self.token_buffer.pop_front().expect("cannot be empty")));
        }

        if self.rest.len() == 0 {
            return None;
        }

        let (rest, _) = opt(Self::parse_empty_line)(self.rest).expect("does not need to match");
        let (mut rest, _) = opt(Self::parse_comment)(rest).expect("does not need to match");

        // Parse indentation. This is only done if we are on a new line, otherwise whitespace when
        // parsing partially through a line will be treated as indentation.
        if self.new_line {
            let (new_rest, current_indent) =
                Self::parse_indent(rest).expect("does not need to match");
            rest = new_rest;

            if current_indent.column == 0 {
                // No indentation
                for _ in 0..self.indent_stack.len() {
                    self.token_buffer.push_back(Token::Dedent);
                }
            } else if self.indent_stack.len() == 0
                || self.indent_stack.last().expect("cannot be empty").column < current_indent.column
            {
                // Deeper indentation
                self.indent_stack.push(current_indent);
                return Some(Ok(Token::Indent));
            } else {
                // Shallower indentation
                while let Some(indent) = self.indent_stack.pop() {
                    if indent.column == current_indent.column {
                        // The indentation results in the same column, but with a different mix of
                        // tabs and spaces
                        if indent != current_indent {
                            todo!("throw mixed tab/space indentation error");
                        }
                        break;
                    }
                    self.token_buffer.push_back(Token::Dedent);
                }
            }

            // Emit the final generated `Indent`/`Dedent` token, or fall through if none were
            // generated (same indentation)
            if let Some(token) = self.token_buffer.pop_front() {
                return Some(Ok(token));
            }
        }

        // Some tokens need to be separated by whitespace, or be at the start of a new line.
        let preceding_space_parser = if self.new_line { space0 } else { space1 };

        let result: nom::IResult<&str, Token, nom::error::Error<&str>> = alt((
            Self::parse_number,
            Self::parse_string,
            // Keyword
            preceded(
                preceding_space_parser,
                // `alt` only supports up to 21 tuple values, hence they have been split up
                alt((
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
                )),
            ),
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
                map(eof, |_| Token::Eof),
                map(char('='), |_| Token::Equals),
                map(char('>'), |_| Token::GreaterThan),
                map(char('{'), |_| Token::LeftBrace),
                map(char('['), |_| Token::LeftBracket),
                map(char('('), |_| Token::LeftParen),
            )),
            alt((
                map(char('<'), |_| Token::LessThan),
                map(line_ending, |_| Token::NewLine),
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
            preceded(preceding_space_parser, Self::parse_identifier),
        ))(rest);
        match result {
            Ok((rest, token)) => {
                // Update position based on how much the length of `rest` decreased
                self.position += self.rest.len() - rest.len();
                self.rest = rest;
                self.new_line = token == Token::NewLine;
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
    Dedent,
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
    Eof,
    Equals,
    Except,
    False,
    Finally,
    Float(&'a str),
    For,
    From,
    Global,
    GreaterThan,
    Identifier(&'a str),
    If,
    Indent,
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
    String(&'a str),
    Tilde,
    True,
    Try,
    While,
    With,
    Yield,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IndentChar {
    Space,
    Tab,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct IndentationLevel {
    pub(crate) column: u8,
    pub(crate) spaces: u8,
    pub(crate) tabs: u8,
}

#[derive(Debug, Diagnostic, Error)]
#[error("error lexing token at line {line}, column {column}")]
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

    macro_rules! assert_tokens_eq {
        ($input:expr, [$($expected:expr),+$(,)?]$(,err { at = $at:expr, line = $line:literal, column = $column:literal })?) => {
            let mut lexer = Lexer::new($input);
            $(
                assert_eq!(lexer.next().unwrap().unwrap(), $expected);
            )+
            $(
                let error = lexer.next().unwrap().unwrap_err();
                assert_eq!(error.at, SourceSpan::from($at));
                assert_eq!(error.line, $line);
                assert_eq!(error.column, $column);
            )?
        };
        ($input:expr, err { at = $at:expr, line = $line:literal, column = $column:literal }) => {
            let mut lexer = Lexer::new($input);
            while let Some(result) = lexer.next() {
                match result {
                    Ok(_) => {},
                    Err(e) => {
                        assert_eq!(e.at, SourceSpan::from($at));
                        assert_eq!(e.line, $line);
                        assert_eq!(e.column, $column);
                        return;
                    },
                }
            }
            panic!("expected error, but none was returned");
        };
    }

    #[test]
    fn iterator_ends() {
        let mut lexer = Lexer::new("10");
        lexer.next().unwrap().unwrap();
        assert!(lexer.next().is_none());
    }

    #[test]
    fn ensure_taking_most_chars_possible() {
        use Token::*;
        assert_tokens_eq!(
            r"***==* None\/",
            [DoubleStar, StarEquals, Equals, Star, None, BackSlash, Slash]
        );
    }

    #[test]
    fn error_location() {
        use Token::*;
        assert_tokens_eq!("*/'foo", [Star, Slash], err { at = 2, line = 1, column = 3 });
    }

    #[test]
    fn numbers() {
        use Token::*;
        assert_tokens_eq!(
            "100-1_0_0_234-1.1",
            [Int("100"), Minus, Int("1_0_0_234"), Minus, Float("1.1")]
        );
        assert_tokens_eq!(
            "--5.2+.6++",
            [Minus, Minus, Float("5.2"), Plus, Float(".6"), Plus, Plus]
        );
        assert_tokens_eq!(
            ".5*83.2_02-1j/",
            [Float(".5"), Star, Complex("83.2_02-1j"), Slash]
        );
        assert_tokens_eq!(
            "3+5J-10_0j+",
            [Complex("3+5J"), Minus, Complex("10_0j"), Plus]
        );
        assert_tokens_eq!(
            "0b1_00-0B10_1+0o70_2-0O2",
            [Int("0b1_00"), Minus, Int("0B10_1"), Plus, Int("0o70_2")]
        );
        assert_tokens_eq!(
            "-0O2+0xFaB4/",
            [Minus, Int("0O2"), Plus, Int("0xFaB4"), Slash]
        );
        assert_tokens_eq!(
            "0XFF_A_D+1_0.34_1+",
            [Int("0XFF_A_D"), Plus, Float("1_0.34_1"), Plus]
        );
        assert_tokens_eq!(
            "0xF0A-1.0_0j-87.7e100+",
            [Complex("0xF0A-1.0_0j"), Minus, Float("87.7e100"), Plus]
        );

        assert_tokens_eq!(
            "12E4-1E-100-",
            [Float("12E4"), Minus, Float("1E-100"), Minus]
        );
        assert_tokens_eq!(
            "01e+100+10e1+10j",
            [Float("01e+100"), Plus, Complex("10e1+10j")]
        );
    }

    #[test]
    fn strings() {
        use Token::*;
        assert_tokens_eq!("10'foo\"'*", [Int("10"), String("'foo\"'"), Star]);
        assert_tokens_eq!("10\"foo'\"*", [Int("10"), String("\"foo'\""), Star]);
        assert_tokens_eq!(
            "10\"\"\"foo\n\"\"\"*",
            [Int("10"), String("\"\"\"foo\n\"\"\""), Star]
        );
        assert_tokens_eq!(
            "10\"\"\"foo\r\n\"\"\"*",
            [Int("10"), String("\"\"\"foo\r\n\"\"\""), Star]
        );
        assert_tokens_eq!("10'''foo\n'''*", [Int("10"), String("'''foo\n'''"), Star]);
        assert_tokens_eq!("10rf'foo{}\"'*", [Int("10"), String("rf'foo{}\"'"), Star]);
        assert_tokens_eq!(
            "10fr'foo{a + b}\"'*",
            [Int("10"), String("fr'foo{a + b}\"'"), Star]
        );
        assert_tokens_eq!(
            "10f'foo{a + b}\"'*",
            [Int("10"), String("f'foo{a + b}\"'"), Star]
        );
        assert_tokens_eq!(
            "10r\"foo{a + b}'\"*",
            [Int("10"), String("r\"foo{a + b}'\""), Star]
        );
        assert_tokens_eq!(
            "10rb'''foo{a + b}\"'''*",
            [Int("10"), String("rb'''foo{a + b}\"'''"), Star]
        );
        assert_tokens_eq!(
            "10br'foo{a + b}\"'*",
            [Int("10"), String("br'foo{a + b}\"'"), Star]
        );
        assert_tokens_eq!(
            "10u'foo{a + b}\"'*",
            [Int("10"), String("u'foo{a + b}\"'"), Star]
        );
        assert_tokens_eq!("10'foo\"*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10\"foo'*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10\"\"\"foo'''*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10'''foo\"\"\"*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10\"foo\n\"*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10'foo\n'*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10fu'foo\"*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10uu'foo\"*", err { at = 2, line = 1, column = 3 });
        assert_tokens_eq!("10ru'foo\"*", err { at = 2, line = 1, column = 3 });
    }

    #[test]
    fn comments() {
        use Token::*;
        assert_tokens_eq!(
            "10*2
# comment  # nested comment
# another line of comments
'foo'  #     another comment
continue
#final comment",
            [
                Int("10"),
                Star,
                Int("2"),
                NewLine,
                NewLine,
                NewLine,
                String("'foo'"),
                NewLine,
                Continue,
                NewLine,
            ]
        );

        assert_tokens_eq!("# comment", [Eof]);
        assert_tokens_eq!("# comment\n", [NewLine]);
        assert_tokens_eq!("# comment\n'foo*", err { at = 10, line = 2, column = 1 });
    }

    #[test]
    fn identifiers() {
        use Token::*;
        assert_tokens_eq!("a = b", [Identifier("a"), Equals, Identifier("b")]);
        assert_tokens_eq!(" A='foo'", [Identifier("A"), Equals, String("'foo'")]);
        assert_tokens_eq!("AbCd1_23 = 2", [Identifier("AbCd1_23"), Equals, Int("2")]);
        assert_tokens_eq!(
            "class Foo:\n\tA = 100",
            [
                Class,
                Identifier("Foo"),
                Colon,
                NewLine,
                Identifier("A"),
                Equals,
                Int("100")
            ]
        );
        assert_tokens_eq!(
            "class Foo:\n\tA = 100",
            [
                Class,
                Identifier("Foo"),
                Colon,
                NewLine,
                Identifier("A"),
                Equals,
                Int("100")
            ]
        );
        assert_tokens_eq!("__init__", [Identifier("__init__")]);
        // TODO: re-enable when Lexing is fixed such that spaces are not tokens, but instead we
        //       enforce there to be spaces between tokens. This should be an error, but currently
        //       isn't (as is the case for many other test cases).
        // assert_tokens_eq!("0foo", err { at = 0, line = 1, column = 2 });
    }
}
