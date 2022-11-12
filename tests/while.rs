mod common;
use jadescript::Jit;

const WHILE: &str = r#"
fn main(a: i32) -> i32 {
	let i = 0
	while i < a {
		i = i + 1
	}
	return i
}
"#;

#[test]
fn while_loop() {
    let mut jit = Jit::default();
    assert_eq!(
        common::run_code::<i32, i32>(&mut jit, WHILE, 24).unwrap(),
        24
    );
}

