#![feature(let_chains)]
mod ast;
mod grammar;
mod jit;

fn main() {
    pretty_env_logger::init();
    println!("{:?}", grammar::parser::file(IF).unwrap());
    let mut jit = jit::Jit::default();
    unsafe { println!("{}", run_code::<i32, bool>(&mut jit, IF, 16).unwrap()) }
}

unsafe fn run_code<I, O>(jit: &mut jit::Jit, code: &str, input: I) -> Result<O, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile(code)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = std::mem::transmute::<_, fn(I) -> O>(code_ptr);
    // And now we can call it!
    Ok(code_fn(input))
}

const TEST_CODE: &str = r#"
fn double(x: f32) -> f32 {
	return x * 4.0
}

fn main(a: f32) -> f32 {
	let mut x: f32 = a + a
	x = double(x)
	return x
}
"#;

const CALL_SYNTAX: &str = r#"
fn double(x: f32) -> f32 {
	return x * 2.0
}

fn main(a: f32) -> f32 {
	let mut x: f32 = a
	x = x.double()
	x = double(x)
	return x
}
"#;

const IF: &str = r#"
fn main(a: i32) -> bool {
	if a >= 0 {
		return true
	} else {
		return false
	}
}
"#;

const IF_2: &str = r#"
fn main(a: i32) -> bool {
	let x: bool = if a >= 0 {
		true
	} else {
		false
	}
	return x
}
"#;

