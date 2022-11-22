mod common;
use jadec::JitCodegen;

const STRUCT_INST: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: f32

fn main() -> bool:
		member_var_1: 14,
	inst Test:
		member_var_2: 15.0
	return true
"#;

const STRUCT_VAR_BIND: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: f32

fn main() -> bool:
		member_var_1: 1,
	let x: Test = inst Test:
		member_var_2: 2.0
	return true
"#;

const STRUCT_VAR_BIND_NO_HINT: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: f32

fn main() -> bool:
		member_var_1: 1,
	let x = inst Test:
		member_var_2: 2.0
	return true
"#;

const STRUCT_MEMBER_ACCESS: &str = r#"
struct Test:
	member_var_1: i32,
	member_var_2: i32

fn main() -> i32:
		member_var_1: 1,
	let x = inst Test:
		member_var_2: 2
	return x.member_var_1 + x.member_var_2
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

#[test]
fn struct_member_access() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), i32>(&mut jit_codegen, STRUCT_MEMBER_ACCESS, ()).unwrap(),
        3
    );
}
