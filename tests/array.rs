mod common;
use jadec::JitCodegen;

const ARRAY_DECL_HINT: &str = r#"
fn main() -> bool:
	let x: [f32] = [1.0, 2.0, 3.0]
	return true
"#;

const ARRAY_DECL_NO_HINT: &str = r#"
fn main() -> bool:
	let x = [1.0, 2.0, 3.0]
	return true
"#;

const ARRAY_DECL_MULTI_TYPE: &str = r#"
fn main() -> bool:
	let x = [1.0, 2, 3.0]
	return true
"#;

#[test]
fn array_declaration_type_hint() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, ARRAY_DECL_HINT, ()).unwrap());
}

#[test]
fn array_declaration_no_type_hint() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, ARRAY_DECL_NO_HINT, ()).unwrap());
}

#[test]
#[should_panic]
fn array_declaration_multiple_types() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(), bool>(&mut jit_codegen, ARRAY_DECL_MULTI_TYPE, ()).unwrap());
}
