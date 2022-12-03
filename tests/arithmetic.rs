mod common;
use jadec::JitCodegen;

const IF_ELSE: &str = r#"
fn main(a: i32, b: i32) -> i32:
	return b + a / 2
"#;

const I32_DIVIDE: &str = r#"
fn main(a: i32, b: i32) -> i32:
	return a / b 
"#;

const U32_DIVIDE: &str = r#"
fn main(a: u32, b: u32) -> u32:
	return a / b 
"#;

const U32_MUL: &str = r#"
fn main(a: u32, b: u32) -> u32:
	return a * b 
"#;

const U32: &str = r#"
fn main(a: u32, b: u32) -> u32:
	return a + b
"#;

const U32_SUB: &str = r#"
fn main(a: u32, b: u32) -> u32:
	return a - b
"#;

#[test]
fn order_of_operations() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(i32, i32), i32>(&mut jit_codegen, IF_ELSE, (10, 5)).unwrap(),
        10
    );
}

#[test]
fn i32_divide_pos() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(i32, i32), i32>(&mut jit_codegen, I32_DIVIDE, (6, 6)).unwrap(),
        1
    );
}

#[test]
fn i32_divide_neg() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(i32, i32), i32>(&mut jit_codegen, I32_DIVIDE, (6, -6)).unwrap(),
        -1
    );
}

#[test]
fn u32_addition() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(u32, u32), u32>(&mut jit_codegen, U32, (6, 6)).unwrap(),
        12
    );
}

#[test]
fn u32_mul() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(u32, u32), u32>(&mut jit_codegen, U32_MUL, (2147483647, 2)).unwrap(),
        4294967294
    );
}

#[test]
fn u32_div() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(u32, u32), u32>(&mut jit_codegen, U32_DIVIDE, (24, 2)).unwrap(),
        12
    );
}

#[test]
fn u32_subtract() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(u32, u32), u32>(&mut jit_codegen, U32_SUB, (6, 6)).unwrap(),
        0
    );
}

#[test]
fn u32_subtract_wrap() {
    let mut jit_codegen = JitCodegen::default();
    assert_eq!(
        common::run_code::<(u32, u32), u32>(&mut jit_codegen, U32_SUB, (6, 7)).unwrap(),
        u32::MAX,
    );
}

// This test can't be used because the program exists instead of panicking.
// #[test]
// #[should_panic]
// fn u32_overflow() {
//     let mut jit_codegen = JitCodegen::default();
//     common::run_code::<(u32, u32), u32>(&mut jit_codegen, U32, (u32::MAX, 1u32)).unwrap();
// }
