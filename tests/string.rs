mod common;
use jadec::JitCodegen;

const STRING_LITERAL: &str = r#"
fn main() -> bool:
	let x: str = "utf-8 string 🏳️‍⚧️"
	return true
"#;

const STRING_LITERAL_NO_HINT: &str = r#"
fn main() -> bool:
	let x = "utf-8 string 🏳️‍⚧️\0"
	return true
"#;

#[test]
fn string_literal() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, STRING_LITERAL, ()).unwrap());
}

#[test]
fn string_literal_no_hint() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, STRING_LITERAL_NO_HINT, ()).unwrap());
}
