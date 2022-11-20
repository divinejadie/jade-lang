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

const STRUCT_VAR_BIND: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: f32

fn main() -> bool:
	let x: Test = Test:
		member_var_1: 1,
		member_var_2: 2.0
	return true
"#;

const STRUCT_VAR_BIND_NO_HINT: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: f32

fn main() -> bool:
	let x = Test:
		member_var_1: 1,
		member_var_2: 2.0
	return true
"#;

#[test]
fn struct_instantiate() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, STRUCT_INST, ()).unwrap());
}

#[test]
fn struct_variable_binding_type_hint() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, STRUCT_VAR_BIND, ()).unwrap());
}

#[test]
fn struct_variable_binding_no_type_hint() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, STRUCT_VAR_BIND_NO_HINT, ()).unwrap());
}

