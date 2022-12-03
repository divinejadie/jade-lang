mod common;
use jadec::JitCodegen;

const U32_LITERAL: &str = r#"
fn main() -> u32:
	return 16u32
"#;

#[test]
fn u32_literal() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), u32>(&mut jit_codegen, U32_LITERAL, ()).unwrap(),
        16
    );
}
