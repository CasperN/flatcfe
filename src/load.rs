use crate::parse;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::io::Read;
use std::path::Path;
use typed_arena::Arena;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileInfo<'a> {
    pub filename: &'a str,
    includes: Vec<&'a str>,
    symbols: Vec<&'a str>,
    imported_symbols: Vec<&'a str>,
}

impl<'a> FileInfo<'a> {
    fn new(filename: &'a str) -> Self {
        FileInfo {
            filename,
            includes: Vec::new(),
            symbols: Vec::new(),
            imported_symbols: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Declaration<'a> {
    pub full_name: &'a str,
    pub defining_schema: &'a str,
    pub parsed_declaration: parse::Declaration<'a>,
}

/// Loads and parses a system of schema files. Tracks which file imported which other file. Ensures
/// fully qualified namespaced symbols are globally unique.
pub fn load<'a>(
    arena: &'a Arena<String>,
    filenames: &'a [String],
) -> (Vec<FileInfo<'a>>, Vec<Declaration<'a>>) {
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
            let parse::Schema {
                included_files,
                declarations,
            } = parse::fully_parse_schema(buf);

            // Record the includes information.
            let schema_info = v.insert(FileInfo::new(schema_file));
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
                            full_name: fully_qualified_name,
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
    (
        schema_files.into_iter().map(|(_, v)| v).collect(),
        symbols.into_iter().map(|(_, v)| v).collect(),
    )
}
