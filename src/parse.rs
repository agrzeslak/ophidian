use crate::lex::Token;

pub struct Parser<'a, I: Iterator<Item = &'a Token<'a>>> {
    tokens: I,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = &'a Token<'a>>,
{
    pub fn new(tokens: I) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> TokenTree<'_> {
        for token in &mut self.tokens {
            println!("{token:?}");
        }
        panic!()
    }

    fn parse_bp(&mut self) -> TokenTree<'_> {
        todo!()
    }
}

pub enum TokenTree<'a> {
    Atom(Atom<'a>),
    Cons(Operand, Vec<TokenTree<'a>>),
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
    Elif, // ?
    Else, // ?
    Equals,
    Except,  // ?
    Finally, // ?
    For,     // ?
    From,
    Global,
    GreaterThan,
    If, // ?
    Import,
    In,
    Is,
    Lambda,
    LessThan,
    Minus,
    MinusEquals,
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
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    Tilde,
    Try,   // ?
    While, // ?
    With,
    Yield,
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
        Break => todo!(),
        Carat => todo!(),
        CaratEquals => todo!(),
        Class => todo!(),
        Colon => todo!(),
        ColonEquals => todo!(),
        Comma => todo!(),
        Continue => todo!(),
        Def => todo!(),
        Del => todo!(),
        Dot => todo!(),
        DoubleGreaterThan => todo!(),
        DoubleGreaterThanEquals => todo!(),
        DoubleLessThan => todo!(),
        DoubleLessThanEquals => todo!(),
        DoubleSlash => todo!(),
        DoubleSlashEquals => todo!(),
        DoubleStar => todo!(),
        DoubleStarEquals => todo!(),
        Elif => todo!(),
        Else => todo!(),
        Equals => todo!(),
        Except => todo!(),
        Finally => todo!(),
        For => todo!(),
        From => todo!(),
        Global => todo!(),
        GreaterThan => todo!(),
        If => todo!(),
        Import => todo!(),
        In => todo!(),
        Is => todo!(),
        Lambda => todo!(),
        LessThan => todo!(),
        Minus => todo!(),
        MinusEquals => todo!(),
        Nonlocal => todo!(),
        Not => todo!(),
        Or => todo!(),
        Pass => todo!(),
        Percent => todo!(),
        PercentEquals => todo!(),
        Plus => todo!(),
        PlusEquals => todo!(),
        Raise => todo!(),
        Return => todo!(),
        Slash => todo!(),
        SlashEquals => todo!(),
        Star => todo!(),
        StarEquals => todo!(),
        Tilde => todo!(),
        Try => todo!(),
        While => todo!(),
        With => todo!(),
        Yield => todo!(),
    }
}
