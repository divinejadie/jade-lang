mod common;
use jadescript::JitCodegen;

const IF_ELSE: &str = r#"fn main(a: i32) -> bool:
	if a >= 0:
		return true
	else:
		return false
"#;

const IF_2: &str = r#"fn main(a: i32) -> bool:
	let x: bool = if a >= 0:
		true
	else:
		false
	return x
"#;

#[test]
fn if_else() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, bool>(&mut jit_codegen, IF_ELSE, 10).unwrap(),
        true
    );
}

#[test]
fn if_else_expr_assign() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, bool>(&mut jit_codegen, IF_2, 10).unwrap(),
        true
    );
}

