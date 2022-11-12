use jadescript::*;

fn main() {
    pretty_env_logger::init();
    println!("{:?}", parser::file(TEST_CODE).unwrap());
    let mut jit = Jit::default();
    unsafe { println!("{}", run_code::<i32, f32>(&mut jit, TEST_CODE, 16).unwrap()) }
}

unsafe fn run_code<I, O>(jit: &mut Jit, code: &str, input: I) -> Result<O, String> {
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

