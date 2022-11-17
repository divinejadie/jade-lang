mod common;
use jadescript::JitCodegen;

const IF_ELSE: &str = r#"
fn main(a: i32, b: i32) -> i32:
	return b + a / 2
"#;

#[test]
fn order_of_operations() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(i32, i32), i32>(&mut jit_codegen, IF_ELSE, (10, 5)).unwrap(),
        10
    );
}

