mod common;
use jadec::JitCodegen;

const F32_I32: &str = r#"
fn main(a: f32) -> i32:
	return a as i32
"#;

const F32_F64: &str = r#"
fn main(a: f32) -> f64:
	return a as f64
"#;

const F32_I64: &str = r#"
fn main(a: f32) -> i64:
	return a as i64
"#;

const I32_I64: &str = r#"
fn main(a: i32) -> i64:
	return a as i64
"#;

const I32_F32: &str = r#"
fn main(a: i32) -> f32:
	return a as f32
"#;

const I32_F64: &str = r#"
fn main(a: i32) -> f64:
	return a as f64
"#;

#[test]
fn f32_as_i32_whole() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, i32>(&mut jit_codegen, F32_I32, 15.0).unwrap(),
        15
    );
}

#[test]
fn f32_as_i32_low() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, i32>(&mut jit_codegen, F32_I32, 15.4).unwrap(),
        15
    );
}

#[test]
fn f32_as_i32_high() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, i32>(&mut jit_codegen, F32_I32, 15.6).unwrap(),
        16
    );
}

#[test]
fn f32_as_f64() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f64>(&mut jit_codegen, F32_F64, 36.66)
            .unwrap()
            .trunc(),
        36.0f64
    );
}

#[test]
fn f32_as_i64() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, i64>(&mut jit_codegen, F32_I64, -36.66).unwrap(),
        -37i64
    );
}

#[test]
fn i32_as_i64() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, i64>(&mut jit_codegen, I32_I64, -24).unwrap(),
        -24i64
    );
}

#[test]
fn i32_as_f32() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, f32>(&mut jit_codegen, I32_F32, -24).unwrap(),
        -24.0
    );
}

#[test]
fn i32_as_f64() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, f64>(&mut jit_codegen, I32_F64, -24).unwrap(),
        -24.0f64
    );
}
