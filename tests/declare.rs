mod common;
use jadescript::JitCodegen;

const HINT: &str = r#"
fn main() -> bool {
	let x: bool = false
	return x
}
"#;

const INFER: &str = r#"
fn main() -> bool {
	let x = false
	return x
}
"#;

#[test]
fn declaration_type_hint() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), bool>(&mut jit_codegen, HINT, ()).unwrap(),
        false
    );
}

#[test]
fn declaration_infer_type() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), bool>(&mut jit_codegen, INFER, ()).unwrap(),
        false
    );
}

