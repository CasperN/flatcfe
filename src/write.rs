use crate::flatc;
use crate::load;
use crate::parse;
use crate::resolve;
use flatbuffers::FlatBufferBuilder;

fn write_metadata<'builder, 'arena>(
    schema_index: usize,
    metadata: &parse::Metadata<'arena>,
    fbb: &mut FlatBufferBuilder<'builder>,
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
    let location = flatc::Location::new(schema_index as u32, metadata.line, metadata.column);
    flatc::Metadata::create(
        fbb,
        &flatc::MetadataArgs {
            documentation,
            attributes,
            location: Some(&location),
        },
    )
}

fn write_type<'b>(
    fbb: &mut FlatBufferBuilder<'b>,
    ty: &resolve::ResolvedType,
) -> flatbuffers::WIPOffset<flatc::Type<'b>> {
    flatc::Type::create(
        fbb,
        &flatc::TypeArgs {
            base_type: ty.base_type,
            element_type: ty.element_type,
            symbol: ty.symbol.map(|s| s as u32),
            fixed_length: ty.fixed_length,
        },
    )
}

fn write_detail<'a, 'b>(
    fbb: &mut FlatBufferBuilder<'b>,
    schema_index: usize,
    detail: &resolve::Detail<'a>,
) -> (
    flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>,
    flatc::Declaration,
) {
    match detail {
        resolve::Detail::Struct { is_struct, fields } => {
            let mut written_fields = Vec::new();
            for field in fields.iter() {
                let name = fbb.create_string(field.field_name);
                let ty = write_type(fbb, &field.field_type);
                let metadata = write_metadata(schema_index, &field.metadata, fbb);
                let default_value = fbb.create_string(field.default_value);
                written_fields.push(flatc::TableField::create(
                    fbb,
                    &flatc::TableFieldArgs {
                        name: Some(name),
                        metadata: Some(metadata),
                        default_value: Some(default_value),
                        non_presence_behavior: flatc::NonPresenceBehavior::Optional, // TODO!
                        type_: Some(ty),
                    },
                ));
            }
            let detail_type = if *is_struct {
                flatc::Declaration::Struct
            } else {
                flatc::Declaration::Table
            };
            let detail = {
                let fields = fbb.create_vector(&written_fields);
                let mut builder = flatc::TableBuilder::new(fbb);
                builder.add_fields(fields);
                builder.finish().as_union_value()
            };
            (detail, detail_type)
        }
        resolve::Detail::Enum {
            is_union,
            enum_type: _,
            variants,
        } => {
            let mut written_variants = Vec::new();
            for variant in variants.iter() {
                let name = fbb.create_string(variant.name);
                let metadata = write_metadata(schema_index, &variant.metadata, fbb);
                let type_ = write_type(fbb, &variant.union_type);
                written_variants.push(flatc::EnumVariant::create(
                    fbb,
                    &flatc::EnumVariantArgs {
                        name: Some(name),
                        metadata: Some(metadata),
                        type_: Some(type_),
                    },
                ));
            }
            let detail_type = if *is_union {
                flatc::Declaration::Enum
            } else {
                flatc::Declaration::Union
            };
            let detail = {
                let variants = fbb.create_vector(&written_variants);
                let mut builder = flatc::EnumBuilder::new(fbb);
                builder.add_variants(variants);
                builder.finish().as_union_value()
            };
            (detail, detail_type)
        }
        resolve::Detail::RpcService { methods } => {
            let mut written_methods = Vec::new();
            for method in methods.iter() {
                let name = fbb.create_string(method.name);
                let metadata = write_metadata(schema_index, &method.metadata, fbb);
                let argument_type = write_type(fbb, &method.argument_type);
                let return_type = write_type(fbb, &method.return_type);
                written_methods.push(flatc::RpcMethod::create(
                    fbb,
                    &flatc::RpcMethodArgs {
                        name: Some(name),
                        metadata: Some(metadata),
                        return_type: Some(return_type),
                        argument_type: Some(argument_type),
                    },
                ));
            }
            let detail_type = flatc::Declaration::RpcService;
            let detail = {
                let methods = fbb.create_vector(&written_methods);
                let mut builder = flatc::RpcServiceBuilder::new(fbb);
                builder.add_methods(methods);
                builder.finish().as_union_value()
            };
            (detail, detail_type)
        }
    }
}

pub fn write_compilation<'a, 'b>(
    fbb: &mut FlatBufferBuilder<'b>,
    symbols: &[resolve::Symbol<'a>],
    schema_files: &[load::FileInfo<'a>],
) {
    let mut written_symbols = Vec::new();
    for symbol in symbols.iter() {
        let name = fbb.create_string(symbol.name);
        let metadata = write_metadata(symbol.schema_index, &symbol.metadata, fbb);
        let (detail, detail_type) = write_detail(fbb, symbol.schema_index, &symbol.detail);
        written_symbols.push(flatc::Symbol::create(
            fbb,
            &flatc::SymbolArgs {
                name: Some(name),
                metadata: Some(metadata),
                detail_type,
                detail: Some(detail),
            },
        ));
    }
    let schemas = {
        let written: Vec<_> = schema_files
            .iter()
            .map(|s| {
                let filename = Some(fbb.create_string(s.filename));
                let mut write = |usizes: &[usize]| {
                    Some(fbb.create_vector_from_iter(usizes.iter().map(|i| *i as u32)))
                };
                let includes = write(&s.includes);
                let imported_symbols = write(&s.imported_symbols);
                let symbols = write(&s.symbols);
                flatc::Schema::create(
                    fbb,
                    &flatc::SchemaArgs {
                        filename,
                        imported_symbols,
                        symbols,
                        includes,
                    },
                )
            })
            .collect();
        fbb.create_vector(&written)
    };
    let features = crate::features::features(symbols);
    let symbols = Some(fbb.create_vector(&written_symbols));
    let compilation = flatc::Compilation::create(
        fbb,
        &flatc::CompilationArgs {
            symbols,
            features,
            schemas: Some(schemas),
            code_gen_options: None,
        },
    );
    fbb.finish(compilation, None);
}
