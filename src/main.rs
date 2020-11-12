#![feature(is_sorted)]
// use indexmap;
use std::io::Read;
use std::path::Path;
// use std::path::PathBuf;
use typed_arena::Arena;
mod parse;
use flatbuffers;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
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
- Restructure the parser types to be Symbol with sub-enum { table, union, enum, struct, etc? }

Phases:
    [x] load_and_parse_schemas
    [x] Freeze SymbolTable and FileTable.
    [ ] write_schemas vector
    [ ] verify_and_write_symbols_vector
    [ ] finish compilation.
*/

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct FileInfo<'a> {
    filename: &'a str,
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
struct Declaration<'a> {
    full_name: &'a str,
    defining_schema: &'a str,
    parsed_declaration: parse::Declaration<'a>,
}

/// Loads and parses a system of schema files. Tracks which file imported which other file. Ensures
/// fully qualified namespaced symbols are globally unique.
fn load<'a>(
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

struct TypeResolvingWriter<'builder, 'arena> {
    fbb: flatbuffers::FlatBufferBuilder<'builder>,
    schema_files: Vec<FileInfo<'arena>>,
    symbols: Vec<Declaration<'arena>>,
}
fn write_metadata<'builder, 'arena>(
    filename: &str,
    metadata: &parse::Metadata<'arena>,
    fbb: &mut flatbuffers::FlatBufferBuilder<'builder>,
    schema_files: &[FileInfo<'arena>],
) -> flatbuffers::WIPOffset<flatc::Metadata<'builder>> {
    let documentation = Some(fbb.create_vector_of_strings(&metadata.documentation.0));
    let attributes = {
        let written_attributes: Vec<_> = metadata
            .attributes
            .iter()
            .map(|parse::KeyVal { key, value }| {
                let key = Some(fbb.create_string(key));
                let value = value.map(|v| fbb.create_string(v));
                flatc::KeyValue::create(fbb, &flatc::KeyValueArgs { key, value })
            })
            .collect();
        Some(fbb.create_vector(&written_attributes))
    };
    let file_index = schema_files
        .binary_search_by(|info| info.filename.cmp(filename))
        .expect("Internal error, did I lose track a schemafile somewhere?")
        as u32;

    let location = flatc::Location::new(file_index, metadata.line, metadata.column);
    flatc::Metadata::create(
        fbb,
        &flatc::MetadataArgs {
            documentation,
            attributes,
            location: Some(&location),
        },
    )
}

fn resolve_primitive_type(ty: &str) -> Option<flatc::BaseType> {
    match ty {
        "string" => Some(flatc::BaseType::String),
        // Classic names.
        "bool" => Some(flatc::BaseType::Bool),
        "ubyte" => Some(flatc::BaseType::UByte),
        "byte" => Some(flatc::BaseType::Byte),
        "ushort" => Some(flatc::BaseType::UShort),
        "short" => Some(flatc::BaseType::Short),
        "uint" => Some(flatc::BaseType::UInt),
        "int" => Some(flatc::BaseType::Int),
        "ulong" => Some(flatc::BaseType::ULong),
        "long" => Some(flatc::BaseType::Long),
        "float" => Some(flatc::BaseType::Float),
        "double" => Some(flatc::BaseType::Double),
        // Modern names.
        "uint8" => Some(flatc::BaseType::UByte),
        "int8" => Some(flatc::BaseType::Byte),
        "uint16" => Some(flatc::BaseType::UShort),
        "int16" => Some(flatc::BaseType::Short),
        "uint32" => Some(flatc::BaseType::UInt),
        "int32" => Some(flatc::BaseType::Int),
        "uint64" => Some(flatc::BaseType::ULong),
        "int64" => Some(flatc::BaseType::Long),
        "float32" => Some(flatc::BaseType::Float),
        "float64" => Some(flatc::BaseType::Double),
        _ => None,
    }
}

fn write_type<'a, 'b>(
    ty: &'a str,
    is_vector: bool,
    caller: &'a str,
    _caller_is_table: bool,
    fbb: &mut flatbuffers::FlatBufferBuilder<'b>,
    symbols: &[Declaration<'a>],
    _schemas: &mut [FileInfo<'a>],
) -> flatbuffers::WIPOffset<flatc::Type<'b>> {
    // what if its a struct!
    let mut builder = flatc::TypeBuilder::new(fbb);

    let base_type = resolve_primitive_type(ty).unwrap_or_else(|| {
        let (_, symbol_index, other_decl) = symbols
            .iter()
            .enumerate()
            .filter_map(|(i, decl)| {
                type_precedence(caller, ty, decl.full_name).map(|p| (p, i, decl))
            })
            .min_by_key(|(p, _, _)| *p)
            .expect(&format!("Couldn't not find type `{}`", &ty));

        builder.add_symbol(symbol_index as u32);

        match other_decl.parsed_declaration {
            parse::Declaration::Table(_) => flatc::BaseType::Struct,
            parse::Declaration::Enum(parse::Enum {
                is_union,
                enum_type,
                ..
            }) => {
                if is_union {
                    flatc::BaseType::Union
                } else {
                    // If the enum's base type isn't an integer primitive, we'll report the error
                    // writing the enum.
                    resolve_primitive_type(enum_type).unwrap_or(flatc::BaseType::None)
                }
            }
            _ => unreachable!(
                "Referencing a symbol `{}` that is not a type",
                other_decl.full_name
            ),
        }
        // update referencing schema.
    });
    if is_vector {
        builder.add_base_type(flatc::BaseType::Vector);
        builder.add_element_type(base_type);
    } else {
        builder.add_base_type(base_type);
    }
    // How to get fixed length?
    builder.finish()
}

impl<'builder, 'arena> TypeResolvingWriter<'builder, 'arena> {
    fn get_file_index(&self, filename: &str) -> usize {
        self.schema_files
            .binary_search_by(|info| info.filename.cmp(filename))
            .expect("Internal error, did I lose track a schemafile somewhere?")
    }
    fn get_file_info(&self, filename: &str) -> &FileInfo<'arena> {
        let i = self.get_file_index(filename);
        &self.schema_files[i]
    }

    fn resolve_type_reference(&self, caller: &str, ty: &str) -> Option<&Declaration<'arena>> {
        // TODO(caspern): This probably shouldn't be an O(N) kinda thing.
        self.symbols
            .iter()
            .filter_map(|decl| type_precedence(caller, ty, decl.full_name).map(|p| (p, decl)))
            .min_by_key(|(p, _)| *p)
            .map(|(_, d)| d)
    }

    fn resolve_and_write_compilation(&mut self) {
        debug_assert!(self.schema_files.is_sorted());

        let mut written_symbols = Vec::new();
        for symbol in self.symbols.iter() {
            use parse::Declaration::*;
            let (name, metadata, detail, detail_type) = match &symbol.parsed_declaration {
                FileExtension(_) | FileIdentifier(_) | Namespace(_) => unreachable!(),
                Table(t) => {
                    let name = self.fbb.create_string(symbol.full_name);
                    let mut written_fields = Vec::new();
                    for field in t.fields.iter() {
                        let name = self.fbb.create_string(field.field_name);
                        let ty = write_type(
                            field.field_type.ident_path.0,
                            field.field_type.is_vector,
                            symbol.full_name,
                            /*caller_is_table=*/ true,
                            &mut self.fbb,
                            &self.symbols,
                            &mut self.schema_files,
                        );
                        let metadata = write_metadata(
                            symbol.defining_schema,
                            &field.metadata,
                            &mut self.fbb,
                            &self.schema_files,
                        );

                        let default_value = if field.default_value.is_some() {
                            Some(self.fbb.create_string(field.default_value.unwrap()))
                        } else {
                            None
                        };
                        let mut builder = flatc::TableFieldBuilder::new(&mut self.fbb);
                        builder.add_name(name);
                        builder.add_type_(ty);
                        builder.add_metadata(metadata);
                        if let Some(v) = default_value {
                            // CASPER: we should populate with 0 like defaults...
                            builder.add_default_value(v);
                        }
                        written_fields.push(builder.finish())
                    }
                    let fields = self.fbb.create_vector(&written_fields);
                    let (detail_type, detail) = if t.is_struct {
                        (
                            flatc::Declaration::Struct,
                            flatc::Table::create(
                                &mut self.fbb,
                                &flatc::TableArgs {
                                    fields: Some(fields),
                                },
                            )
                            .as_union_value(),
                        )
                    } else {
                        (
                            flatc::Declaration::Table,
                            flatc::Table::create(
                                &mut self.fbb,
                                &flatc::TableArgs {
                                    fields: Some(fields),
                                },
                            )
                            .as_union_value(),
                        )
                    };
                    let metadata = write_metadata(
                        symbol.defining_schema,
                        &t.metadata,
                        &mut self.fbb,
                        &self.schema_files,
                    );
                    (name, metadata, detail, detail_type)
                }
                Enum(e) => {
                    let name = self.fbb.create_string(symbol.full_name);
                    let metadata = write_metadata(
                        symbol.defining_schema,
                        &e.metadata,
                        &mut self.fbb,
                        &self.schema_files,
                    );
                    let mut written_variants = Vec::new();
                    for variant in e.variants.iter() {
                        let name = self.fbb.create_string(variant.name);
                        let metadata = write_metadata(
                            symbol.defining_schema,
                            &variant.metadata,
                            &mut self.fbb,
                            &mut self.schema_files,
                        );
                        // let value = variant.value.map(|v| self.fbb.create_string(v));
                        let mut builder = flatc::EnumVariantBuilder::new(&mut self.fbb);
                        builder.add_name(name);
                        builder.add_metadata(metadata);
                        // if let Some(v) = value {
                        //     builder.add_value(v);
                        // }
                        written_variants.push(builder.finish());
                    }
                    let variants = self.fbb.create_vector(&written_variants);
                    let (detail_type, detail) = {
                        (
                            flatc::Declaration::Enum,
                            flatc::Enum::create(
                                &mut self.fbb,
                                &flatc::EnumArgs {
                                    variants: Some(variants),
                                },
                            )
                            .as_union_value(),
                        )
                    };
                    (name, metadata, detail, detail_type)
                }
                RpcService(r) => {
                    let name = self.fbb.create_string(symbol.full_name);
                    let metadata = write_metadata(
                        symbol.defining_schema,
                        &r.metadata,
                        &mut self.fbb,
                        &self.schema_files,
                    );
                    let (detail_type, detail) = {
                        (
                            flatc::Declaration::RpcService,
                            flatc::RpcService::create(&mut self.fbb, &Default::default())
                                .as_union_value(),
                        )
                    };
                    (name, metadata, detail, detail_type)
                }
            };
            let mut symbol_builder = flatc::SymbolBuilder::new(&mut self.fbb);
            symbol_builder.add_name(name);
            symbol_builder.add_metadata(metadata);
            symbol_builder.add_detail_type(detail_type);
            symbol_builder.add_detail(detail);
            written_symbols.push(symbol_builder.finish());
        }

        let s = self.fbb.create_vector(&written_symbols);
        let c = flatc::Compilation::create(
            &mut self.fbb,
            &flatc::CompilationArgs {
                symbols: Some(s),
                ..Default::default()
            },
        );
        self.fbb.finish(c, None);
    }
}

/// If `caller` calling `ty` may resolve to `symbol` return Some(p) where p is the precidence.
/// Assumes `caller` and `symbol` are fully qualified types from the root namespace.
/// The symbol with the lowest precidence is the one that will be selected.
fn type_precedence(caller: &str, ty: &str, symbol: &str) -> Option<usize> {
    if !symbol.ends_with(ty) {
        return None;
    }
    // Drop ty suffix from symbol.
    let mut symbol_prefix = {
        let prefix = &symbol[..symbol.len() - ty.len()];
        if prefix.ends_with(".") {
            prefix[..prefix.len() - 1].split('.')
        } else {
            let mut s = prefix.split('.');
            if prefix.is_empty() {
                s.next(); // empty the iterator.
            }
            s
        }
    };
    let mut caller_prefix = caller.split('.');
    caller_prefix.next_back(); // Drop the type, leaving the namespace.
    loop {
        match (symbol_prefix.next(), caller_prefix.next()) {
            // Sharing the same namespace so far, maybe we'll match.
            (Some(_), Some(_)) => continue,
            // Symbol is in a higher or eq namespace
            // e.g. symbol prefix = A.B.C ,  caller prefix = A.B.C.D
            (None, Some(_)) => return Some(caller_prefix.count() + 1),
            (None, None) => return Some(0),
            // Symbol is in a lower namespace than caller prefix => not accessible.
            // e.g. symbol prefix = A.B.C.D, caller prefix = A.B.C
            (Some(_), None) => return None,
        }
    }
}

#[test]
fn test_type_precedence() {
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.B.C.D.E.struct"),
        Some(0)
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.B.C.E.struct"),
        Some(1)
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.B.E.struct"),
        Some(2)
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.E.struct"),
        Some(3)
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "E.struct"),
        Some(4)
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.F.struct"),
        None
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.B.C.D.table"),
        None
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.B.C.D.F.E.struct"),
        None
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "A.B.C.D.FE.struct"),
        None
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "E.struct", "lol.some.irrelevant.type"),
        None
    );
    assert_eq!(
        type_precedence("A.B.C.D.table", "C.struct", "A.B.C.struct"),
        Some(2) // Precedence is 2 b/c A.B.C.D.C.struct and A.B.C.C.struct are better.
    );
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
    let (schema_files, symbols) = load(&arena, &filepaths);

    let mut tw = TypeResolvingWriter {
        schema_files,
        symbols,
        fbb: flatbuffers::FlatBufferBuilder::new(),
    };
    tw.resolve_and_write_compilation();

    let buf = tw.fbb.finished_data();
    let c = flatbuffers::get_root::<flatc::Compilation>(buf);
    dbg!(c);
}
