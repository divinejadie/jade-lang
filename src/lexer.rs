#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Identifier(String),
    Literal(String),

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

pub fn indent_process(tokens: Vec<Token>) -> Vec<Token> {
    let mut out = vec![];
    let mut level: u32 = 0;
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
                while n < levels.len() as u32 {
                    out.push(Token::Dedent);
                    level = levels.pop().unwrap();
                    if level < n {
                        panic!("FUCK");
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
        / identifier()
        / type_literal()

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
        = d:$(['0'..='9' | '.']+) { Token::Literal(d.to_string()) }
        / "true" { Token::Literal("true".to_string()) }
        / "false" { Token::Literal("false".to_string()) }
});

