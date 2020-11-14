use crate::flatc;
use crate::load;
use crate::parse;
use crate::types::resolve_primitive_type;
use crate::types::type_precedence;

pub struct ResolvedType {
    // TODO: replace with object API?
    pub base_type: flatc::BaseType,
    pub element_type: flatc::BaseType,
    // True for tables and unions. BaseType doesn't model this for reasons...
    pub has_references: bool,
    pub symbol: Option<usize>,
    pub fixed_length: u16,
}
pub struct TableField<'a> {
    pub field_name: &'a str,
    pub field_type: ResolvedType,
    pub metadata: parse::Metadata<'a>,
    pub default_value: &'a str,
}
pub struct EnumVariant<'a> {
    pub name: &'a str,
    pub metadata: parse::Metadata<'a>,
    pub union_type: Option<ResolvedType>,
    pub value: i64,
}
pub struct RpcMethod<'a> {
    pub name: &'a str,
    pub metadata: parse::Metadata<'a>,
    pub argument_type: ResolvedType,
    pub return_type: ResolvedType,
}
pub enum Detail<'a> {
    Struct {
        is_struct: bool,
        fields: Vec<TableField<'a>>,
    },
    Enum {
        is_union: bool,
        enum_type: flatc::BaseType,
        variants: Vec<EnumVariant<'a>>,
    },
    RpcService {
        methods: Vec<RpcMethod<'a>>,
    },
}
pub struct Symbol<'a> {
    pub name: &'a str,
    pub symbol_index: usize,
    pub schema_index: usize,
    pub metadata: parse::Metadata<'a>,
    pub detail: Detail<'a>,
}

// Typechecks all symbols, updates symbol and schema references to use `usize` instead of strings.
pub fn resolve<'a>(
    mut schemas: Vec<load::FileInfo<'a>>,
    symbols: Vec<load::Declaration<'a>>,
) -> (Vec<load::FileInfo<'a>>, Vec<Symbol<'a>>) {
    let mut resolved_symbols = Vec::new();
    for i in 0..symbols.len() {
        resolved_symbols.push(resolve_symbol(i, &symbols, &mut schemas));
    }
    (schemas, resolved_symbols)
}

fn resolve_table_field<'a>(
    table: &load::Declaration<'a>,
    parsed_table_field: &parse::TableField<'a>,
    symbols: &[load::Declaration<'a>],
    schemas: &mut [load::FileInfo<'_>],
) -> TableField<'a> {
    let parse::TableField {
        field_name,
        field_type:
            parse::TableFieldType {
                ident_path: parse::IdentifierPath(type_identifier),
                is_vector,
            },
        metadata,
        default_value: parsed_default_value,
    } = parsed_table_field;

    let field_type = resolve_type(
        type_identifier,
        table.full_name,
        *is_vector,
        symbols,
        schemas,
    );
    // Make sure structs don't contain tables or unions.
    if let parse::Detail::Struct { is_struct, .. } = table.parsed_symbol.detail {
        if is_struct && field_type.has_references {
            println!(
                "Structs cannot contain tables or unions.\n\
                 Error at field `{}` defined in file:'{}' at line:{} and column:{}.",
                field_name, table.defining_schema, metadata.line, metadata.column,
            );
            std::process::exit(1);
        }
    } else {
        unreachable!();
    }
    // TODO: Casper: typecheck the default value and resolve it with an appropriate scalar default.
    let default_value = parsed_default_value.unwrap_or("0");
    TableField {
        field_name,
        field_type,
        metadata: metadata.clone(),
        default_value,
    }
}

fn resolve_enum_variant<'a>(
    union_symbol: &load::Declaration<'a>,
    parsed_enum_variant: &parse::EnumVariant<'a>,
    is_union: bool,
    symbols: &[load::Declaration<'a>],
    schemas: &mut [load::FileInfo<'_>],
) -> EnumVariant<'a> {
    let parse::EnumVariant {
        name,
        union_type,
        metadata,
        value: _,
    } = parsed_enum_variant;

    // Resolve union types. CASPER: maybe move this branch elsewhere?
    let union_type = match (is_union, union_type) {
        (false, None) => None,
        (false, Some(_)) => {
            println!(
                "Enum variants do not have types.\n\
                 Error at variant `{}` defined in file:'{}' at line:{} and column:{}.",
                name, union_symbol.defining_schema, metadata.line, metadata.column,
            );
            std::process::exit(1);
        }
        (true, ut) => {
            let (type_path, is_vector) = if let Some(t) = ut {
                (t.ident_path.0, t.is_vector)
            } else {
                // If unspecified, the union variant's type name is its name.
                // TODO: to update variant name if its a qualified union variant
                // union X { A.B.Y } => A_B_Y variant.
                (*name, false)
            };
            Some(resolve_type(
                type_path,
                union_symbol.full_name,
                is_vector,
                symbols,
                schemas,
            ))
        }
    };

    // TODO(cneo): Resolve enum value.
    EnumVariant {
        name,
        metadata: metadata.clone(),
        union_type,
        value: 0,
    }
}

fn resolve_rpc_method<'a>(
    rpc_service: &load::Declaration<'a>,
    parsed_rpc_method: &parse::RpcMethod<'a>,
    symbols: &[load::Declaration<'a>],
    schemas: &mut [load::FileInfo<'_>],
) -> RpcMethod<'a> {
    let parse::RpcMethod {
        name,
        metadata,
        argument_type: parsed_argument_type,
        return_type: parsed_return_type,
    } = parsed_rpc_method;
    let mut resolve = |ty| resolve_type(ty, rpc_service.full_name, false, symbols, schemas);
    let argument_type = resolve(parsed_argument_type.0);
    let return_type = resolve(parsed_return_type.0);
    RpcMethod {
        name,
        metadata: metadata.clone(),
        argument_type,
        return_type,
    }
}

fn resolve_symbol<'a>(
    symbol_index: usize,
    symbols: &[load::Declaration<'a>],
    schemas: &mut [load::FileInfo<'_>],
) -> Symbol<'a> {
    let declaration = &symbols[symbol_index];
    let load::Declaration {
        full_name,
        defining_schema,
        parsed_symbol:
            parse::Symbol {
                name: _,
                metadata,
                detail: parsed_detail,
            },
    } = declaration;
    let schema_index = schemas
        .binary_search_by_key(&defining_schema, |s| &s.filename)
        .unwrap();

    let detail = match parsed_detail {
        parse::Detail::Struct { is_struct, fields } => {
            let resolved_fields = fields
                .iter()
                .map(|field| {
                    let rt = resolve_table_field(&declaration, field, symbols, schemas);
                    if *is_struct && rt.field_type.has_references {
                        println!(
                            "Structs cannot contain tables, vectors, or unions.\n\
                             Error at field `{}` defined in file:'{}' at line:{} and column:{}.",
                            rt.field_name, defining_schema, metadata.line, metadata.column,
                        );
                        std::process::exit(1);
                    }
                    rt
                })
                .collect();
            Detail::Struct {
                is_struct: *is_struct,
                fields: resolved_fields,
            }
        }
        parse::Detail::RpcService { methods } => {
            let resolved_methods = methods
                .iter()
                .map(|method| {
                    let rm = resolve_rpc_method(&declaration, method, symbols, schemas);
                    if !(rm.argument_type.is_table() && rm.return_type.is_table()) {
                        println!(
                            "Rpc methods must have tables for argument and return types. \n\
                             Error at method `{}` defined in file:'{}' at line:{} and column:{}.",
                            rm.name, defining_schema, metadata.line, metadata.column,
                        );
                        std::process::exit(1);
                    }
                    rm
                })
                .collect();
            Detail::RpcService {
                methods: resolved_methods,
            }
        }
        parse::Detail::Enum {
            is_union,
            variants,
            enum_type,
        } => {
            let resolved_variants = variants
                .iter()
                .map(|variant| {
                    let rv =
                        resolve_enum_variant(&declaration, variant, *is_union, symbols, schemas);
                    rv
                })
                .collect();

            let enum_type = if enum_type.is_empty() {
                flatc::BaseType::Union
            } else {
                let bt = resolve_primitive_type(enum_type).unwrap_or_else(|| {
                    println!(
                        "Could not resolve enum underlying type `{}`.\n\
                         Error at declaration `{}` defined in file:'{}' at line:{} and column:{}",
                        enum_type, full_name, defining_schema, metadata.line, metadata.column
                    );
                    std::process::exit(1)
                });
                if bt.0 < 1 || bt.0 > 10 {
                    println!(
                        "Enum base_types must be (unsigned) integral types, not {:?}.\n\
                         Error at declaration `{}` defined in file:'{}' at line:{} and column:{}",
                        enum_type, full_name, defining_schema, metadata.line, metadata.column
                    );
                    std::process::exit(1)
                };
                bt
            };
            Detail::Enum {
                is_union: *is_union,
                enum_type,
                variants: resolved_variants,
            }
        }
    };

    Symbol {
        name: full_name,
        symbol_index,
        schema_index,
        metadata: metadata.clone(),
        detail,
    }
}

impl ResolvedType {
    fn is_table(&self) -> bool {
        self.base_type == flatc::BaseType::Struct && self.has_references
    }
}

fn resolve_type(
    ty: &str,
    caller: &str, // need caller_schemafile_index
    is_vector: bool,
    symbols: &[load::Declaration<'_>],
    _schemas: &mut [load::FileInfo<'_>], // CASPER: Calling schema needs to be updated with who it imported.
) -> ResolvedType {
    let (ty, has_references, symbol) = if let Some(base_type) = resolve_primitive_type(ty) {
        let has_references = base_type == flatc::BaseType::String;
        (base_type, has_references, None)
    } else {
        let (_, symbol_index, other_decl) = symbols
            .iter()
            .enumerate()
            .filter_map(|(i, decl)| {
                type_precedence(caller, ty, decl.full_name).map(|p| (p, i, decl))
            })
            .min_by_key(|(p, _, _)| *p)
            .expect(&format!("Couldn't not find type `{}`", &ty));

        use parse::Detail::*;
        let (ty, has_references) = match other_decl.parsed_symbol.detail {
            Struct { is_struct, .. } => (flatc::BaseType::Struct, !is_struct),
            Enum {
                is_union,
                enum_type,
                ..
            } => {
                if is_union {
                    (flatc::BaseType::Union, true)
                } else {
                    // If the enum's base type isn't an integer primitive, we'll report the error
                    // writing the enum declaration.
                    (
                        resolve_primitive_type(enum_type).unwrap_or(flatc::BaseType::None),
                        false,
                    )
                }
            }
            RpcService { .. } => unreachable!(
                "`{}` is referencing an RpcService as a type",
                other_decl.full_name
            ),
        };
        (ty, has_references, Some(symbol_index))
    };
    let (base_type, element_type) = if is_vector {
        (flatc::BaseType::Vector, ty)
    } else {
        (ty, flatc::BaseType::None)
    };
    ResolvedType {
        base_type,
        element_type,
        fixed_length: 0,
        has_references: has_references || is_vector,
        symbol,
    }
}
