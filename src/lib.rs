#![feature(is_sorted)]
pub use compilation_generated::flatbuffers_compiler as flatc;
use load::*;
use typed_arena::Arena;

#[allow(clippy::all)] // Generated code.
#[rustfmt::skip]
mod compilation_generated;
mod features;
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
    - Magic type field for unions.
    - Validate table default values.
- Feature detection.
- ParserErrors
    - Readable errors
- Testing.
    - End-to-end tests with test schemas?
    - Scrub TODOs
- Flags
    - Input code_gen_options
    - Output to json? or debug vs binary output
- Deal with weird declarations:
    - root type and file_id, file_ext?
*/

pub fn compile(filepaths: &[impl AsRef<str>], fbb: &mut flatbuffers::FlatBufferBuilder) {
    let arena = Arena::new();
    let (schema_files, declarations) = load::load(&arena, filepaths);
    let (symbols, schema_files) = resolve::resolve(schema_files, declarations);
    write::write_compilation(fbb, &schema_files, &symbols);
}
