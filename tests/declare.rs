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

const MANY_DECL: &str = r#"
fn main() -> bool {
	let x = false
	let y = 32.0
	let z = 32.0
	let a = 32.0
	let b = 32.0
	let c = 32.0
	let d = 32.0
	let e = 32.0
	let f = 32.0
	let g = 32.0
	let h = 32.0
	let i = 32.0
	let j = 32.0
	let k = 32.0
	let l = 32.0
	let m = 32.0
	let n = 32.0
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

#[test]
fn declaration_many() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), bool>(&mut jit_codegen, MANY_DECL, ()).unwrap(),
        false
    );
}

