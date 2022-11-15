mod common;
use jadescript::AotCodegen;

#[test]
fn module() {
    let mut aot_codegen = AotCodegen::default();
    aot_codegen.compile_project(
        &std::path::PathBuf::from("tests/jadescript/example.jadescript"),
        &std::path::PathBuf::from("tests/jadescript/example.out"),
    );
}

