mod ast;
mod grammar;

fn main() {
    println!(
        "{:?}",
        grammar::parser::function(
            r#"fn foo(a, b) -> str {
            let c = if a {
                if b {
                    30
                } else {
                    40
                }
            } else {
                50
            }
            let x: f32 = 2.2
            let string: str = "String Literal"
            c = c + 2
            return c
        }
        "#
        )
        .unwrap()
    );
}

