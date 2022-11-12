use jadescript::Jit;

pub fn run_code<I, O>(jit: &mut Jit, code: &str, input: I) -> Result<O, String> {
    let code_ptr = jit.compile(code)?;
    let code_fn = unsafe { std::mem::transmute::<_, fn(I) -> O>(code_ptr) };
    Ok(code_fn(input))
}

