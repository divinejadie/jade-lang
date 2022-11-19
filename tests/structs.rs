mod common;
use jadescript::JitCodegen;

const STRUCT_INST: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: f32

fn main() -> bool:
	Test:
		member_var_1: 14,
		member_var_2: 15.0
	return true
"#;

#[test]
fn struct_instantiate() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, STRUCT_INST, ()).unwrap(),);
}

