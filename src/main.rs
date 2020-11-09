// use indexmap;
use std::io::Read;
use std::path::Path;
// use std::path::PathBuf;
use typed_arena::Arena;
mod parse;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use flatbuffers;
mod compilation_generated;
use compilation_generated::flatbuffers_compiler as flatc;

/*
TODO:
Resolve full names of all symbols
    - Construct symbol map
    - Make sure things are defined once
- Resolve all type references
    - Type references should be fully qualified
    - While at it, process special keywords on table fields
    - And check types make sense
        - How to check table with a default enum variant?
    - Write to compilation.fbs
- Typecheck all fields make sense.
- Understand special annotations
    - id, deprecated, required, key
    - id needs to be inferred.
- Write compilation.fbs
    - construct who-imports-who map
- ParserErrors
    - Location tracking.
    - Readable errors
- Flags? Input code_gen_options?
- Wtf with root type and file_id, file_ext?

Phases:
    [x] load_and_parse_schemas
    [ ] Freeze SymbolTable and FileTable.
    [ ] write_schemas vector
    [ ] verify_and_write_symbols_vector
    [ ] finish compilation.
*/

#[derive(Default, Debug)]
struct FileInfo<'a> {
    includes: Vec<&'a str>,
    symbols: Vec<&'a str>,
    imported_symbols: Vec<&'a str>,
}
#[derive(Debug)]
struct Declaration<'a> {
    defining_schema: &'a str,
    parsed_declaration: parse::Declaration<'a>,
}

/// Maps schema filenames to metadata of symbols in the file.
#[derive(Debug)]
struct SchemaFileTable<'a>(BTreeMap<&'a str, FileInfo<'a>>);

/// Maps fully namespaced symbol names to their declarations.
#[derive(Debug)]
struct SymbolTable<'a>(BTreeMap<&'a str, Declaration<'a>>);

/// Loads and parses a system of schema files. Tracks which file imported which other file. Ensures
/// fully qualified namespaced symbols are globally unique.
fn load<'a>(
    arena: &'a Arena<String>,
    filenames: &'a [String],
) -> (SchemaFileTable<'a>, SymbolTable<'a>) {
    let mut to_visit = std::collections::VecDeque::<&'a str>::new();
    let mut schema_files = BTreeMap::<&'a str, FileInfo<'a>>::new();
    let mut symbols = BTreeMap::<&'a str, Declaration<'a>>::new();
    for f in filenames {
        to_visit.push_back(arena.alloc(f.to_string()));
    }
    while let Some(schema_file) = to_visit.pop_front() {
        if let std::collections::btree_map::Entry::Vacant(v) = schema_files.entry(&schema_file) {
            // Load the file.
            let path = Path::new(schema_file);
            let mut f = std::fs::File::open(path).expect("failed to open file.");
            let buf = arena.alloc(String::new());
            f.read_to_string(buf).expect("failed to read file");
            let (
                rest,
                parse::Schema {
                    included_files,
                    declarations,
                },
            ) = parse::schema(buf).expect("Parse Error.");
            assert_eq!(rest, "", "Did not fully parse schema.");

            // Record the includes information.
            let schema_info = v.insert(FileInfo::default());
            for included in included_files.iter() {
                to_visit.push_back(included);
                schema_info.includes.push(included);
            }
            // Record all declarations by their fully qualified namespaced names.
            let mut current_namespace = String::new();
            for decl in declarations.iter() {
                use parse::Declaration::*;
                let name = match decl {
                    Namespace(ident) => {
                        current_namespace.clear();
                        current_namespace.push_str(ident.0);
                        continue;
                    }
                    Table(t) => t.name,
                    Enum(e) => e.name,
                    RpcService(r) => r.name,
                    FileExtension(_) | FileIdentifier(_) => {
                        // CASPER what to do with these?
                        continue;
                    }
                };
                let fully_qualified_name = arena.alloc(current_namespace.clone());
                fully_qualified_name.push('.');
                fully_qualified_name.push_str(name);
                // Associate symbol with schema file.
                schema_info.symbols.push(fully_qualified_name);
                // Store in symbol table.
                match symbols.entry(fully_qualified_name) {
                    Entry::Vacant(e) => {
                        e.insert(Declaration {
                            defining_schema: schema_file,
                            parsed_declaration: decl.clone(),
                        });
                    }
                    Entry::Occupied(e) => {
                        panic!("Duplicated symbol: {:?}", e.key());
                    }
                }
            }
        }
    }
    (SchemaFileTable(schema_files), SymbolTable(symbols))
}

fn resolve_and_write<'a>(
    fbb: &mut flatbuffers::FlatBufferBuilder,
    _schema_files: &mut SchemaFileTable<'a>,
    symbols: &SymbolTable<'a>
) {
    let mut written_symbols = Vec::new();

    for (_symbol, decl) in symbols.0.iter() {
        use parse::Declaration::*;
        match &decl.parsed_declaration {
            FileExtension(_) | FileIdentifier(_) | Namespace(_) => unreachable!(),
            Table(t) => {
                let documentation = Some(fbb.create_vector_of_strings(&t.documentation.0));
                let mut attributes = Vec::new();
                for parse::KeyVal { key, value } in &t.attributes {
                    let key = Some(fbb.create_string(key));
                    let value = value.map(|v| fbb.create_string(v));
                    let pair = flatc::KeyValue::create(fbb,
                        &flatc::KeyValueArgs {
                            key, value
                        }
                    );
                    attributes.push(pair);
                }
                let attributes = Some(fbb.create_vector(&attributes));
                // TODO: Location info.
                let metadata = Some(flatc::Metadata::create(fbb, &flatc::MetadataArgs {
                    documentation, attributes, ..Default::default()
                }));
                let name = Some(fbb.create_string(t.name));
                let symbol = flatc::Symbol::create(fbb, &flatc::SymbolArgs {
                    name, metadata, ..Default::default()
                });
                written_symbols.push(symbol);
            },
            _ => {}
        }
    }
    let s = fbb.create_vector(&written_symbols);
    let c = flatc::Compilation::create(fbb, &flatc::CompilationArgs {
        symbols: Some(s),
        ..Default::default()
    });
    fbb.finish(c, None);
}



fn main() {
    let mut args = std::env::args();
    if args.len() == 1 {
        println!("Usage: ./flatcfe [schema.fbs]+");
        return;
    }
    args.next(); // Skip program name.
    let filepaths: Vec<_> = args.collect();

    let arena = Arena::new();
    let (mut schema_file_table, symbol_table) = dbg!(load(&arena, &filepaths));
    let fbb = &mut flatbuffers::FlatBufferBuilder::new();
    resolve_and_write(fbb, &mut schema_file_table, &symbol_table);

    let buf = fbb.finished_data();
    let c = flatbuffers::get_root::<flatc::Compilation>(buf);
    dbg!(c);
}
