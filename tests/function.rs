mod common;
use jadec::JitCodegen;

const CODE: &str = r#"
fn double(x: f32) -> f32:
	return x * 2.0

fn main(a: f32) -> f32:
	let mut x: f32 = a
	x = double(x)
	return x
"#;

const CODE_2: &str = r#"
fn double(x: f32) -> f32:
	return x * 2.0

fn main(a: f32) -> f32:
	let mut x: f32 = a
	x = x.double()
	return x
"#;

const NO_RETURN: &str = r#"
fn main():
	return
"#;

const RECURSION: &str = r#"
fn until_zero(num: i32) -> i32:
	if num <= 0:
		return num
	else:
		return until_zero(num - 1)

fn main(input: i32) -> i32:
	return until_zero(input)
"#;

const OUT_OF_ORDER: &str = r#"
fn main(a: f32) -> f32:
	let mut x: f32 = a
	x = double(x)
	return x

fn double(x: f32) -> f32:
	return x * 2.0
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

#[test]
fn recursion() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, i32>(&mut jit_codegen, RECURSION, 10).unwrap(),
        0
    );
}

#[test]
fn out_of_order() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, OUT_OF_ORDER, 10.0).unwrap(),
        20.0
    );
}
