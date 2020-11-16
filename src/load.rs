use crate::parse;
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Read;
use std::path::Path;
use typed_arena::Arena;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileInfo<'a> {
    pub filename: &'a str,
    pub includes: Vec<usize>,
    pub symbols: Vec<usize>,
    pub imported_symbols: Vec<usize>,
}

#[derive(Debug)]
pub struct Declaration<'a> {
    pub full_name: &'a str,
    pub defining_schema: &'a str,
    pub parsed_symbol: parse::Symbol<'a>,
}

/// Loads and parses a system of schema files. Tracks which file imported which other file. Ensures
/// fully qualified namespaced symbols are globally unique.
pub fn load<'a>(
    arena: &'a Arena<String>,
    filenames: &'a [String],
) -> (Vec<FileInfo<'a>>, Vec<Declaration<'a>>) {
    let mut to_visit = std::collections::VecDeque::<&'a str>::new();
    let mut schema_files = BTreeMap::<&'a str, BTreeSet<&'a str>>::new();
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
            v.insert(included_files.iter().cloned().collect());
            to_visit.extend(included_files.iter());

            // Record all declarations by their fully qualified namespaced names.
            let mut current_namespace = String::new();
            for decl in declarations.iter() {
                use parse::Declaration::*;
                let symbol = match decl {
                    Symbol(s) => s,
                    Namespace(ident) => {
                        current_namespace.clear();
                        current_namespace.push_str(ident.0);
                        continue;
                    }
                    FileExtension(_) | FileIdentifier(_) => {
                        // CASPER what to do with these?
                        continue;
                    }
                };
                let fully_qualified_name = arena.alloc(current_namespace.clone());
                fully_qualified_name.push('.');
                fully_qualified_name.push_str(symbol.name);
                // Store in symbol table.
                match symbols.entry(fully_qualified_name) {
                    Entry::Vacant(e) => {
                        e.insert(Declaration {
                            full_name: fully_qualified_name,
                            defining_schema: schema_file,
                            parsed_symbol: symbol.clone(), // CASPER: no clone?
                        });
                    }
                    Entry::Occupied(e) => {
                        panic!("Duplicated symbol: {:?}", e.key());
                    }
                }
            }
        }
    }
    // Collect to vector so we can use binary_search_by and get the index of a file.
    let schema_files: Vec<_> = schema_files.into_iter().collect();
    debug_assert!(schema_files.is_sorted_by_key(|(k, _)| *k));
    let includes = schema_files
        .iter()
        .map(|(filename, included_filenames)| {
            let mut include_indices: Vec<_> = included_filenames
                .iter()
                .map(|f| schema_files.binary_search_by_key(f, |(f, _)| f).unwrap())
                .collect();
            include_indices.sort_unstable();
            FileInfo {
                filename,
                includes: include_indices,
                symbols: Vec::new(),
                imported_symbols: Vec::new(),
            }
        })
        .collect();
    (includes, symbols.into_iter().map(|(_, v)| v).collect())
}
