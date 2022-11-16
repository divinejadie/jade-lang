use jadescript::lexer;
use lexer::Token;

const CODE: &str = r#"fn main() -> f32:
	return 1.5"#;

const CODE_2: &str = r#"fn main() -> f32:
	let x = 1.5
	return x
"#;

#[test]
fn lexer() {
    let tokens = lexer::lexer::lex(CODE).unwrap();
    assert!(
        tokens
            == vec![
                Token::Function,
                Token::Space,
                Token::Identifier(String::from("main")),
                Token::OpenParenthesis,
                Token::CloseParenthesis,
                Token::Space,
                Token::Minus,
                Token::GreaterThan,
                Token::Space,
                Token::Identifier(String::from("f32")),
                Token::Colon,
                Token::NewLine,
                Token::Indent,
                Token::Return,
                Token::Space,
                Token::Literal(String::from("1.5"))
            ]
    );
}

#[test]
fn indent_pass() {
    let tokens: Vec<Token> = lexer::lexer::lex(CODE_2).unwrap();
    println!("{:#?}", tokens);
    let tokens = lexer::indent_process(tokens);
    println!("{:#?}", tokens);
    assert!(
        tokens
            == vec![
                Token::Function,
                Token::Space,
                Token::Identifier(String::from("main")),
                Token::OpenParenthesis,
                Token::CloseParenthesis,
                Token::Space,
                Token::Minus,
                Token::GreaterThan,
                Token::Space,
                Token::Identifier(String::from("f32")),
                Token::Colon,
                Token::NewLine,
                Token::Indent,
                Token::Let,
                Token::Space,
                Token::Identifier(String::from("x")),
                Token::Space,
                Token::Equals,
                Token::Space,
                Token::Literal(String::from("1.5")),
                Token::NewLine,
                Token::Return,
                Token::Space,
                Token::Identifier(String::from("x")),
                Token::NewLine,
                Token::Dedent,
            ]
    );
}

