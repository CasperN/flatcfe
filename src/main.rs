use std::io::Read;
use std::path::{Path, PathBuf};
use typed_arena::Arena;
use indexmap;
mod parse;
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
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
    load_and_parse_schemas
    Freeze SymbolTable and FileTable.
      Resolve insertion-order-index to alphabetical-order-index
    write_schemas vector
    verify_and_write_symbols_vector
    finish compilation.

*/

struct FileInfo {
    includes: Vec<FileNumber>,
    symbols: Vec<SymbolNumber>,
    imported_symbols: Vec<SymbolNumber>,
}
// The index of a file in visitation order.
struct FileNumber(usize);
struct FileTable {
    files: indexmap::IndexMap<PathBuf, FileInfo>,
    // The next file to visit.
    next: usize,
}

// The index of a symbol in visitation order.
struct SymbolInfo<'arena> {
    declaration: parse::Declaration<'arena>,
    declared_by: FileNumber,
}
struct SymbolNumber(usize);
struct SymbolTable<'arena> {
    symbols: indexmap::IndexMap<String, SymbolInfo<'arena>>,
}


/*
fn load(&mut self, filenames: &[&Path]) {
    let mut schema_files = indexmap::IndexMap::new();
    for f in filenames.iter() {
        schema_files.insert(f.to_path_buf(), FileInfo::default());
    }
    let mut filenumber = 0;

    while let Some(filename) = schema_files.get_index(filenumber) {
        filenumber += 1;
        if self.files.contains_key(&filename) {
            continue;
        }
        let mut f = std::fs::File::open(&filename).expect("failed to open file.");
        let buf = self.arena.alloc(String::new());
        f.read_to_string(buf).expect("failed to read file");
        let (rest, schema) = parse::schema(buf).expect("Parse Error.");
        assert_eq!(rest, "");
        for included in schema.included_files.iter() {
            filenames.push(Path::new(included).to_path_buf())
        }
        self.files.insert(filename, schema);
    }
}
*/


struct Schemas<'arena> {
    // Since pared schemas are backed by the string they parsed, and we have an unknown number
    // of schemas to read, we put them in an arena for stable lifetimes. Another approach would
    // for parsers that copy but the overhead of many little string allocations seems worse than
    // the cost of storing all the schema files in memory at the same time.
    arena: &'arena Arena<String>,
    files: BTreeMap<PathBuf, parse::Schema<'arena>>,
}
impl<'arena> Schemas<'arena> {
    fn new(arena: &'arena Arena<String>) -> Self {
        Schemas {
            arena,
            files: BTreeMap::new(),
        }
    }
    // this should be Resulty.
    fn load(&mut self, mut filenames: Vec<PathBuf>) {
        while let Some(filename) = filenames.pop() {
            if self.files.contains_key(&filename) {
                continue;
            }
            let mut f = std::fs::File::open(&filename).expect("failed to open file.");
            let buf = self.arena.alloc(String::new());
            f.read_to_string(buf).expect("failed to read file");
            let (rest, schema) = parse::schema(buf).expect("Parse Error.");
            assert_eq!(rest, "");
            for included in schema.included_files.iter() {
                filenames.push(Path::new(included).to_path_buf())
            }
            self.files.insert(filename, schema);
        }
    }
}
// CASPER: we should be able to move out of schemas so long as we track the includes map somewhere.
struct Symbols<'arena>(BTreeMap<String, parse::Declaration<'arena>>);
impl<'arena> Symbols<'arena> {
    /// Walk down a schema and look at all declarations and give them full names.
    fn resolve_names(&mut self, schema: &parse::Schema<'arena>) {
        let mut current_namespace = String::new();
        for decl in schema.declarations.iter() {
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
            let mut fully_qualified_name = current_namespace.clone();
            fully_qualified_name.push('.');
            fully_qualified_name.push_str(name);
            match self.0.entry(fully_qualified_name) {
                Entry::Vacant(e) => {e.insert(decl.clone());},
                Entry::Occupied(e) => {
                    panic!("Duplicated symbol: {:?}", e.key());
                }
            }
        }
    }
}

fn main() {
    let mut args = std::env::args();
    if args.len() == 1 {
        println!("Usage: ./flatcfe [schema.fbs]+");
        return;
    }
    args.next(); // Skip program name.
    let filepaths: Vec<_> = args.map(|f| Path::new(&f).to_owned()).collect();
    let arena = Arena::new();
    let mut schemas = Schemas::new(&arena);
    schemas.load(filepaths);

    let mut symbols = Symbols(BTreeMap::new());
    for s in schemas.files.values() {
        symbols.resolve_names(s);
    }
    dbg!("{:?}", symbols.0);
}
