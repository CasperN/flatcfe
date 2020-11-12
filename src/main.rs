#![feature(is_sorted)]
use typed_arena::Arena;
use flatbuffers;
use compilation_generated::flatbuffers_compiler as flatc;
use load::*;
use types::*; // CASPER get rid of glob import.

mod types;
mod load;
mod parse;
mod compilation_generated;

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
    [x] write_schemas vector
    [ ] verify_and_write_symbols_vector
    [ ] finish compilation.
*/

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

struct TypeReference<'a> {
    // Need reference to caller metadata and file_info.
    ty: &'a str,
    caller: &'a str,
    caller_is_struct: bool,
    is_vector: bool,
}

fn write_type<'a, 'b>(
    type_reference: TypeReference<'a>,
    fbb: &mut flatbuffers::FlatBufferBuilder<'b>,
    symbols: &[Declaration<'a>],
    _schemas: &mut [FileInfo<'a>],  // CASPER: Calling schema needs to be updated with who it imported.
) -> flatbuffers::WIPOffset<flatc::Type<'b>> {
    let TypeReference {
        ty,
        caller,
        caller_is_struct,
        is_vector,
    } = type_reference;
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
            parse::Declaration::Table(_) => {
                if caller_is_struct {
                    println!(
                        "Structs `{}` cannot have tables as fields `{}`",
                        caller, &other_decl.full_name
                    );
                    std::process::exit(1);
                }
                flatc::BaseType::Struct
            }
            parse::Declaration::Enum(parse::Enum {
                is_union,
                enum_type,
                ..
            }) => {
                if is_union {
                    if caller_is_struct {
                        println!(
                            "Structs `{}` cannot have unions as fields `{}`",
                            caller, &other_decl.full_name
                        );
                         // CASPER: Make things result typed and exit gracefully.
                        std::process::exit(1);
                    }
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
                            TypeReference {
                                ty: field.field_type.ident_path.0,
                                caller: symbol.full_name,
                                is_vector: field.field_type.is_vector,
                                caller_is_struct: t.is_struct,
                            },
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
                    let detail_type = if t.is_struct {
                        flatc::Declaration::Struct
                    } else {
                        flatc::Declaration::Table
                    };
                    let detail = {
                        let fields = self.fbb.create_vector(&written_fields);
                        let mut builder = flatc::TableBuilder::new(&mut self.fbb);
                        builder.add_fields(fields);
                        builder.finish().as_union_value()
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
                        let mut builder = flatc::EnumVariantBuilder::new(&mut self.fbb);
                        builder.add_name(name);
                        builder.add_metadata(metadata);
                        written_variants.push(builder.finish());
                    }
                    // CASPER enums need base types.
                    let detail_type = if e.is_union {
                        flatc::Declaration::Enum
                    } else {
                        flatc::Declaration::Union
                    };
                    let detail = {
                        let variants = self.fbb.create_vector(&written_variants);
                        let mut builder = flatc::EnumBuilder::new(&mut self.fbb);
                        builder.add_variants(variants);
                        builder.finish().as_union_value()
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
                    let mut written_methods = Vec::new();
                    for method in r.methods.iter() {
                        let name = self.fbb.create_string(method.name);
                        let metadata = write_metadata(
                            symbol.defining_schema,
                            &method.metadata,
                            &mut self.fbb,
                            &mut self.schema_files,
                        );
                        let argument_type = write_type(
                            TypeReference {
                                ty: method.argument_type.0,
                                caller: symbol.full_name,
                                is_vector: false,
                                caller_is_struct: false,
                            },
                            &mut self.fbb,
                            &self.symbols,
                            &mut self.schema_files,
                        );
                        let return_type = write_type(
                            TypeReference {
                                ty: method.return_type.0,
                                caller: symbol.full_name,
                                is_vector: false,
                                caller_is_struct: false,
                            },
                            &mut self.fbb,
                            &self.symbols,
                            &mut self.schema_files,
                        );
                        let mut builder = flatc::RpcMethodBuilder::new(&mut self.fbb);
                        builder.add_name(name);
                        builder.add_metadata(metadata);
                        builder.add_return_type(return_type);
                        builder.add_argument_type(argument_type);
                        written_methods.push(builder.finish());
                    }
                    let detail_type = flatc::Declaration::RpcService;
                    let detail = {
                        let methods = self.fbb.create_vector(&written_methods);
                        let mut builder = flatc::RpcServiceBuilder::new(&mut self.fbb);
                        builder.add_methods(methods);
                        builder.finish().as_union_value()
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

        let compilation = {
            let symbols = self.fbb.create_vector(&written_symbols);
            let mut builder = flatc::CompilationBuilder::new(&mut self.fbb);
            builder.add_symbols(symbols);
            builder.finish()
        };
        self.fbb.finish(compilation, None);
    }
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
    let (schema_files, symbols) = load::load(&arena, &filepaths);

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
