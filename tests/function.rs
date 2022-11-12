mod common;
use jadescript::Jit;

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

#[test]
fn function_call() {
    let mut jit = Jit::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit, CODE, 10.0).unwrap(),
        20.0
    );
}

#[test]
fn ufcs_method_call() {
    let mut jit = Jit::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit, CODE_2, 10.0).unwrap(),
        20.0
    );
}

