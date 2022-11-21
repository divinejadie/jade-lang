mod common;
use jadec::AotCodegen;

#[test]
fn module() {
    let mut aot_codegen = AotCodegen::default();
    aot_codegen.compile_project(
        &std::path::PathBuf::from("tests/jade/example.jade"),
        &std::path::PathBuf::from("tests/jade/example.out"),
    );
}
