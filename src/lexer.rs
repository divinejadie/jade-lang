use crate::ast::TypeLiteral;
use peg::{Parse, ParseElem, ParseLiteral, ParseSlice};

trait Stringy {
    fn contains_string(&self) -> Option<&str>;
}

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Literal(TypeLiteral),

    Function,
    While,
    If,
    Else,
    Return,
    Let,
    Mut,
    Module,
    Struct,
    And,
    Or,
    Xor,
    Not,

    OpenParenthesis,
    CloseParenthesis,
    Colon,
    Equals,
    GreaterThan,
    LessThan,
    Slash,
    Plus,
    Minus,
    ExclaimationMark,
    Asterisk,
    Period,
    Comma,
    Ampersand,

    Space,
    Indent,
    Dedent,
    NewLine,
}

// Eq for a token will ONLY be checking the discriminant and NOT any inner types
impl std::cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant::<Token>(self) == std::mem::discriminant::<Token>(other)
    }
}

impl std::cmp::Eq for Token {}

impl Stringy for Token {
    fn contains_string(&self) -> Option<&str> {
        match self {
            Token::Identifier(l) => Some(&l),
            _ => None,
        }
    }
}

pub struct SliceByRef<'a, T>(pub &'a [T]);

impl<'a, T> Parse for SliceByRef<'a, T> {
    type PositionRepr = usize;

    fn start(&self) -> usize {
        0
    }
    fn is_eof(&self, pos: usize) -> bool {
        pos >= self.0.len()
    }
    fn position_repr(&self, p: usize) -> Self::PositionRepr {
        p
    }
}

impl<'a, T: 'a> ParseElem<'a> for SliceByRef<'a, T> {
    type Element = &'a T;
    fn parse_elem(&'a self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self.0[pos..].first() {
            Some(r) => peg::RuleResult::Matched(pos + 1, r),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'a, T: Stringy> ParseLiteral for SliceByRef<'a, T> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        if let Some(lit) = self.0[pos].contains_string() {
            let l = literal.len();
            if lit.len() >= pos + l && &lit.as_bytes()[pos..pos + l] == literal.as_bytes() {
                peg::RuleResult::Matched(pos + l, ())
            } else {
                peg::RuleResult::Failed
            }
        } else {
            peg::RuleResult::Failed
        }
    }
}

impl<'a, T> ParseSlice<'a> for SliceByRef<'a, T> {
    type Slice = &'a [T];
    fn parse_slice(&'a self, p1: usize, p2: usize) -> &'a [T] {
        &self.0[p1..p2]
    }
}

pub(crate) fn indent_process(tokens: Vec<Token>) -> Vec<Token> {
    let mut out = vec![];
    let mut levels = vec![];
    let mut iter = tokens.iter();

    while let Some(token) = iter.next() {
        match token {
            Token::NewLine => {
                out.push(Token::NewLine);
                let mut n: u32 = 0;
                let mut fallthrough = None;
                while let Some(t) = iter.next() {
                    if Token::Indent == *t {
                        n += 1;
                    } else {
                        fallthrough = Some(t.clone());
                        break;
                    }
                }
                if n > levels.len() as u32 {
                    out.push(Token::Indent);
                    levels.push(n);
                }
                let mut level;
                while n < levels.len() as u32 {
                    out.push(Token::Dedent);
                    level = levels.pop().unwrap();
                    if level < n {
                        panic!("Malformed token stream");
                    }
                }
                if let Some(t) = fallthrough {
                    out.push(t);
                }
            }
            _ => out.push(token.clone()),
        }
    }
    return out;
}

peg::parser!(pub grammar lexer() for str {
    pub rule lex() -> Vec<Token>
        = token()*

    rule token() -> Token
        = keyword()
        / spacing()
        / punctuation()
        / type_literal()
        / identifier()

    rule keyword() -> Token
        = "fn" { Token::Function }
        / "struct" { Token::Struct }
        / "module" { Token::Module }
        / "let" { Token::Let }
        / "mut" { Token::Mut }
        / "return" { Token::Return }
        / "if" { Token::If }
        / "else" { Token::Else }
        / "while" { Token::While }
        / "and" { Token::And }
        / "or" { Token::Or }
        / "xor" { Token::Xor }
        / "not" { Token::Not }

    rule punctuation() -> Token
        = "=" { Token::Equals }
        / "+" { Token::Plus }
        / "-" { Token::Minus }
        / "*" { Token::Asterisk }
        / "/" { Token::Slash }
        / "." { Token::Period }
        / ":" { Token::Colon }
        / "," { Token::Comma }
        / "!" { Token::ExclaimationMark }
        / "&" { Token::Ampersand }
        / "<" { Token::LessThan }
        / ">" { Token::GreaterThan }
        / "(" { Token::OpenParenthesis }
        / ")" { Token::CloseParenthesis }

    rule spacing() -> Token
        = "\t" { Token::Indent }
        / "\n" { Token::NewLine }
        / " " { Token::Space }

    rule identifier() -> Token
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Token::Identifier(n.to_owned()) } }

    rule type_literal() -> Token
        = d:$(['0'..='9']+) "." e:$(['0'..='9']+) { Token::Literal(TypeLiteral::F32(format!("{}.{}", d, e).parse::<f32>().unwrap())) }
        / n:$(['0'..='9']+) { Token::Literal(TypeLiteral::I32(n.parse::<i32>().unwrap())) }
        / "true" { Token::Literal(TypeLiteral::Bool(true)) }
        / "false" { Token::Literal(TypeLiteral::Bool(false)) }
});

