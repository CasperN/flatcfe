use flatbuffers::{get_root, FlatBufferBuilder};
use flatcfe::{compile, flatc};

fn cd_tests() {
    std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/tests")).unwrap();
}

#[test]
fn test_imports() {
    cd_tests();
    let fbb = &mut FlatBufferBuilder::new();
    compile(&["test_imports/s1.fbs"], fbb);

    let c = get_root::<flatc::Compilation>(fbb.finished_data());
    let symbols = c.symbols().unwrap();
    let schemas = c.schemas().unwrap();
    assert_eq!(schemas.len(), 2);
    let schema1 = c.schemas().unwrap().get(0);
    let schema2 = c.schemas().unwrap().get(1);

    // Test filenames.
    assert_eq!(schema1.filename().unwrap(), "test_imports/s1.fbs");
    assert_eq!(schema2.filename().unwrap(), "test_imports/s2.fbs");

    // Schema1 imports schema2 and schema2 imports nothing.
    assert_eq!(schema1.includes().unwrap().len(), 1);
    assert_eq!(schema1.includes().unwrap().get(0), 1);
    assert!(schema2.includes().unwrap().is_empty());

    // Test the symbols.
    assert_eq!(symbols.get(0).name(), "A.Bar");
    assert_eq!(symbols.get(1).name(), "A.Baz");
    assert_eq!(symbols.get(2).name(), "A.Foo");
    assert_eq!(symbols.get(3).name(), "B.Bar");

    // Test where symbols were declared.
    let s1_symbols = schema1.symbols().unwrap();
    assert_eq!(s1_symbols.len(), 2);
    assert_eq!(s1_symbols.get(0), 0);  // A.Bar
    assert_eq!(s1_symbols.get(1), 2);  // A.Foo

    let s2_symbols = schema2.symbols().unwrap();
    assert_eq!(s2_symbols.len(), 2);
    assert_eq!(s2_symbols.get(0), 1);  // A.Baz
    assert_eq!(s2_symbols.get(1), 3);  // B.Bar

    // Schema1 imports A.Bar and B.Bar
    // TODO!!!!
    // let s1_imports = schema1.imported_symbols().unwrap();
    // assert_eq!(s1_imports.get(0), 0);
    // assert_eq!(s1_imports.get(1), 3);

    // Schema2 does not import.
    assert!(schema2.imported_symbols().unwrap().is_empty());

}
