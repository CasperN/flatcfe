#![feature(is_sorted)]
use compilation_generated::flatbuffers_compiler as flatc;
use flatbuffers::FlatBufferBuilder;
use load::*;
use typed_arena::Arena;

mod compilation_generated;
mod load;
mod parse;
mod resolve;
mod types;
mod write;

/*
TODO:
- Understand special annotations
    - id, deprecated, required, key
    - id needs to be inferred.
- ParserErrors
    - Location tracking.
    - Readable errors
- Flags
    - Input code_gen_options
    - Output to json? or debug vs binary output
- Wtf with root type and file_id, file_ext?
- Restructure the parser types to be Symbol with sub-enum { table, union, enum, struct, etc? }

- Array support.
- Write FileInfo
- untie type resolution from writing?
*/

fn main() {
    let mut args = std::env::args();
    if args.len() == 1 {
        println!("Usage: ./flatcfe [schema.fbs]+");
        return;
    }
    args.next(); // Skip program name.
    let filepaths: Vec<_> = args.collect();

    let arena = Arena::new();
    let (schema_files, symbols) = load::load(&arena, &filepaths);
    let (symbols, schema_files) = resolve::resolve(schema_files, symbols);

    let mut fbb = FlatBufferBuilder::new();
    write::write_compilation(&mut fbb, &schema_files, &symbols);

    let buf = fbb.finished_data();
    let c = flatbuffers::get_root::<flatc::Compilation>(buf);
    // dbg!(c);
    println!("{:?}", buf);
}
