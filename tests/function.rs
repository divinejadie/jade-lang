mod common;
use jadescript::JitCodegen;

const CODE: &str = r#"
fn double(x: f32) -> f32 {
	return x * 2.0
}

fn main(a: f32) -> f32 {
	let mut x: f32 = a
	x = double(x)
	return x
}
"#;

const CODE_2: &str = r#"
fn double(x: f32) -> f32 {
	return x * 2.0
}

fn main(a: f32) -> f32 {
	let mut x: f32 = a
	x = x.double()
	return x
}
"#;

const NO_RETURN: &str = r#"
fn main() {
	return
}
"#;

#[test]
fn function_call() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, CODE, 10.0).unwrap(),
        20.0
    );
}

#[test]
fn ufcs_method_call() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, CODE_2, 10.0).unwrap(),
        20.0
    );
}

#[test]
fn no_return() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(), ()>(&mut jit_codegen, NO_RETURN, ()).unwrap(),
        ()
    );
}

