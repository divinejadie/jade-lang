mod common;
use jadescript::Jit;

const AND: &str = r#"
fn main(a: bool, b: bool) -> bool {
	return a and b
}
"#;

const OR: &str = r#"
fn main(a: bool, b: bool) -> bool {
	return a or b
}
"#;

const XOR: &str = r#"
fn main(a: bool, b: bool) -> bool {
	return a xor b
}
"#;

const NOT: &str = r#"
fn main(a: bool) -> bool {
	return not a
}
"#;

#[test]
fn not() {
    let mut jit = Jit::default();
    assert!(common::run_code::<bool, bool>(&mut jit, NOT, false).unwrap(),);
}

#[test]
fn and_true() {
    let mut jit = Jit::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit, AND, (true, true)).unwrap(),);
}

#[test]
fn and_true_false() {
    let mut jit = Jit::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit, AND, (true, false)).unwrap(),);
}

#[test]
fn and_false() {
    let mut jit = Jit::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit, AND, (false, false)).unwrap(),);
}

#[test]
fn or_true() {
    let mut jit = Jit::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit, OR, (true, true)).unwrap(),);
}

#[test]
fn or_false() {
    let mut jit = Jit::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit, OR, (false, false)).unwrap(),);
}

#[test]
fn or_true_false() {
    let mut jit = Jit::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit, OR, (true, false)).unwrap(),);
}

#[test]
fn xor_true() {
    let mut jit = Jit::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit, XOR, (true, true)).unwrap(),);
}

#[test]
fn xor_false() {
    let mut jit = Jit::default();
    assert!(!common::run_code::<(bool, bool), bool>(&mut jit, XOR, (false, false)).unwrap(),);
}

#[test]
fn xor_true_false() {
    let mut jit = Jit::default();
    assert!(common::run_code::<(bool, bool), bool>(&mut jit, XOR, (true, false)).unwrap(),);
}

