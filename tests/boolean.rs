mod common;
use jadescript::JitCodegen;

const AND: &str = r#"
fn main(a: bool, b: bool) -> bool:
	return a and b
"#;

const OR: &str = r#"
fn main(a: bool, b: bool) -> bool:
	return a or b
"#;

const XOR: &str = r#"
fn main(a: bool, b: bool) -> bool:
	return a xor b
"#;

const NOT: &str = r#"
fn main(a: bool) -> bool:
	return not a
"#;

#[test]
fn not() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<bool, bool>(&mut jit_codegen, NOT, false).unwrap(),);
}

#[test]
fn and_true() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit_codegen, AND, (true, true)).unwrap(),);
}

#[test]
fn and_true_false() {
    let mut jit_codegen = JitCodegen::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit_codegen, AND, (true, false)).unwrap(),);
}

#[test]
fn and_false() {
    let mut jit_codegen = JitCodegen::default();
    assert!(
        !common::run_code::<(bool, bool), bool>(&mut jit_codegen, AND, (false, false)).unwrap(),
    );
}

#[test]
fn or_true() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit_codegen, OR, (true, true)).unwrap(),);
}

#[test]
fn or_false() {
    let mut jit_codegen = JitCodegen::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit_codegen, OR, (false, false)).unwrap(),);
}

#[test]
fn or_true_false() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit_codegen, OR, (true, false)).unwrap(),);
}

#[test]
fn xor_true() {
    let mut jit_codegen = JitCodegen::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit_codegen, XOR, (true, true)).unwrap(),);
}

#[test]
fn xor_false() {
    let mut jit_codegen = JitCodegen::default();
    assert!(
        !common::run_code::<(bool, bool), bool>(&mut jit_codegen, XOR, (false, false)).unwrap(),
    );
}

#[test]
fn xor_true_false() {
    let mut jit_codegen = JitCodegen::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit_codegen, XOR, (true, false)).unwrap(),);
}

