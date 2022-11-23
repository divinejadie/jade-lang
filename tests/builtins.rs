mod common;
use jadec::JitCodegen;

const SQRT_UFCS: &str = r#"
fn main(a: f32) -> f32:
	return a.sqrt()
"#;

const SQRT_F32: &str = r#"
fn main(a: f32) -> f32:
	return sqrt(a)
"#;

const SQRT_F64: &str = r#"
fn main(a: f64) -> f64:
	return sqrt(a)
"#;

const ABS_F32: &str = r#"
fn main(a: f32) -> f32:
	return abs(a)
"#;

const ABS_F64: &str = r#"
fn main(a: f64) -> f64:
	return abs(a)
"#;

const ABS_I32: &str = r#"
fn main(a: i32) -> i32:
	return abs(a)
"#;

const ABS_I64: &str = r#"
fn main(a: i64) -> i64:
	return abs(a)
"#;

#[test]
fn builtin_ufcs() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, SQRT_UFCS, 16.0).unwrap(),
        4.0
    );
}

#[test]
fn sqrt_f32() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, SQRT_F32, 16.0).unwrap(),
        4.0
    );
}

#[test]
fn sqrt_f64() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f64, f64>(&mut jit_codegen, SQRT_F64, 16.0).unwrap(),
        4.0
    );
}

#[test]
fn abs_f64_neg() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f64, f64>(&mut jit_codegen, ABS_F64, -16.345).unwrap(),
        16.345
    );
}

#[test]
fn abs_f64_pos() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f64, f64>(&mut jit_codegen, ABS_F64, 16.345).unwrap(),
        16.345
    );
}

#[test]
fn abs_f32_neg() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, ABS_F32, -16.345).unwrap(),
        16.345
    );
}

#[test]
fn abs_f32_pos() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<f32, f32>(&mut jit_codegen, ABS_F32, 16.345).unwrap(),
        16.345
    );
}

#[test]
fn abs_i32_pos() {
    pretty_env_logger::init();
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, i32>(&mut jit_codegen, ABS_I32, 12i32).unwrap(),
        12i32
    );
}

#[test]
fn abs_i32_neg() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i32, i32>(&mut jit_codegen, ABS_I32, -12i32).unwrap(),
        12i32
    );
}

#[test]
fn abs_i64_pos() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i64, i64>(&mut jit_codegen, ABS_I64, 12).unwrap(),
        12
    );
}

#[test]
fn abs_i64_neg() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<i64, i64>(&mut jit_codegen, ABS_I64, -12).unwrap(),
        12
    );
}
