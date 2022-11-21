use jadec::*;

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "Jade")]
#[command(author = "Amelia Joison")]
#[command(version = "0.1")]
#[command(about =  "Compile and JIT Jade", long_about = None)]
pub struct Args {
    pub file: PathBuf,

    pub out: PathBuf,
}

// STEPS TO RUN:
// 1. jadec example.jade out.o
// 2. mold out.o -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib64/libc.so.6
// 3. ./a.out
fn main() {
    pretty_env_logger::init();
    let args = Args::parse();
    let mut codegen = AotCodegen::default();

    codegen.compile_project(&args.file, &args.out);
}
