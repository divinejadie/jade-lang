mod common;
use jadec::JitCodegen;

const STRUCT_INST: &str = r#"
struct Test:
	member_var_1: i32
	member_var_2: f32

fn main() -> bool:
	inst Test:
		member_var_1: 14
		member_var_2: 15.0
	return true
"#;

const STRUCT_VAR_BIND: &str = r#"
struct Test:
	member_var_1: i32
	member_var_2: f32

fn main() -> bool:
	let x: Test = inst Test:
		member_var_1: 1
		member_var_2: 2.0
	return true
"#;

const STRUCT_VAR_BIND_NO_HINT: &str = r#"
struct Test:
	member_var_1: i32
	member_var_2: f32

fn main() -> bool:
	let x = inst Test:
		member_var_1: 1
		member_var_2: 2.0
	return true
"#;

const STRUCT_MEMBER_ACCESS: &str = r#"
struct Test:
	member_var_1: i32
	member_var_2: i32

fn main() -> i32:
	let x = inst Test:
		member_var_1: 1
		member_var_2: 2
	return x.member_var_1 + x.member_var_2
"#;

const STRUCT_IN_STRUCT: &str = r#"
struct InnerStruct:
	item: i32

struct OuterStruct:
	inner: InnerStruct
	other_field: i32

fn main() -> i32:
	let inner = inst InnerStruct:
		item: 9
	let s = inst OuterStruct:
		inner: inner
		other_field: 12
	return s.inner.item
"#;

const STRUCT_INSTANTIATE_IN_STRUCT: &str = r#"
struct OuterStruct:
	inner: InnerStruct
	other_field: i32

struct InnerStruct:
	item: i32

fn main() -> i32:
	let s = inst OuterStruct:
		inner: inst InnerStruct:
			item: 9
		other_field: 12
	return s.inner.item
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

#[test]
fn struct_in_struct() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), i32>(&mut jit_codegen, STRUCT_IN_STRUCT, ()).unwrap(),
        9,
    );
}

#[test]
fn struct_instantiate_in_struct() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), i32>(&mut jit_codegen, STRUCT_INSTANTIATE_IN_STRUCT, ()).unwrap(),
        9,
    );
}
