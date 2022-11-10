#![feature(let_chains)]
mod ast;
mod grammar;
mod jit;

fn main() {
    println!("{:?}", grammar::parser::function(TEST_CODE).unwrap());

    let mut jit = jit::Jit::default();
    unsafe {
        println!(
            "{}",
            run_code::<f32, f32>(&mut jit, TEST_CODE, 10.0).unwrap()
        )
    }
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

const TEST_CODE: &str = r#"fn main(a: f32) -> f32 {
        let x: f32 = a + a
        return x
    }"#;

