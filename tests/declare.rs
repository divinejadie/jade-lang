mod common;
use jadescript::Jit;

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
    let mut jit = Jit::default();
    assert_eq!(
        common::run_code::<(), bool>(&mut jit, HINT, ()).unwrap(),
        false
    );
}

#[test]
fn declaration_infer_type() {
    let mut jit = Jit::default();
    assert_eq!(
        common::run_code::<(), bool>(&mut jit, INFER, ()).unwrap(),
        false
    );
}

