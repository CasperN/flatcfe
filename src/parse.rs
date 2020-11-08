use nom;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while};
use nom::character::complete::{alpha1, char, multispace0, multispace1, none_of, one_of};
use nom::combinator::{map, opt, peek, recognize, value};
use nom::multi::{many0, separated_list};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

#[cfg(test)]
#[path = "./parse_test.rs"]
mod test;

// Skips multispace0 and comments.
fn skip_ws<'s, T>(
    parser: impl Fn(&'s str) -> IResult<&str, T>,
) -> impl Fn(&'s str) -> IResult<&'s str, T> {
    // Note we use multispace1 which matches on at least 1 whitespace.
    // many0 with multispace0 together will loop forever.
    let whitespace = value((), multispace1);
    // eol comment does not match block comments so block comments in the wrong places
    // are errors.
    let eol_comment = value((), tuple((tag("//"), none_of("/"), is_not("\n\r"))));
    let block_comment = value((), tuple((tag(r#"/*"#), take_until(r#"*/"#), tag(r#"*/"#))));
    let skip = many0(alt((whitespace, eol_comment, block_comment)));
    preceded(skip, parser)
}

fn string_literal(s: &str) -> IResult<&str, &str> {
    let escaped_string = nom::bytes::complete::escaped(is_not("\"\\"), '\\', one_of("\"\\"));
    delimited(skip_ws(char('"')), escaped_string, char('"'))(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Documentation<'s>(Vec<&'s str>);
fn documentation_block(s: &str) -> IResult<&str, Documentation> {
    let doc = preceded(skip_ws(tag("///")), is_not("\n\r"));
    map(many0(doc), Documentation)(s)
}

fn identifier(s: &str) -> IResult<&str, &str> {
    let head = alt((alpha1, tag("_")));
    let body = take_while(|c: char| c.is_ascii_alphanumeric() || c == '_');
    preceded(peek(head), body)(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IdentifierPath<'s>(pub &'s str);
fn identifier_path(s: &str) -> IResult<&str, IdentifierPath> {
    map(
        recognize(separated_list(
            nom::character::complete::char('.'),
            identifier,
        )),
        IdentifierPath,
    )(s)
}

fn delimited_by_chars<'s, T>(
    left: char,
    parser: impl Fn(&'s str) -> IResult<&'s str, T>,
    right: char,
) -> impl Fn(&'s str) -> IResult<&'s str, T> {
    delimited(skip_ws(char(left)), skip_ws(parser), skip_ws(char(right)))
}

// TODO: parse single value. (value is not an identifier)
fn maybe_eq_value(s: &str) -> IResult<&str, Option<&str>> {
    opt(preceded(skip_ws(char('=')), skip_ws(identifier)))(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct KeyVal<'s> {
    key: &'s str,
    value: Option<&'s str>,
}
fn keyval(schema: &str) -> IResult<&str, KeyVal<'_>> {
    let (rest, key) = preceded(multispace0, identifier)(schema)?;
    let (rest, value) = maybe_eq_value(rest)?;
    Ok((rest, KeyVal { key, value }))
}
// Returns an empty list if there's no list.
fn maybe_keyvals(schema: &str) -> IResult<&str, Vec<KeyVal<'_>>> {
    let open = preceded(multispace0, char('('));
    let sep = preceded(multispace0, char(','));
    let close = preceded(multispace0, char(')'));
    map(
        opt(delimited(open, separated_list(sep, keyval), close)),
        |v| v.unwrap_or_default(),
    )(schema)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TableFieldType<'s> {
    ident_path: IdentifierPath<'s>,
    is_vector: bool,
}
fn field_type(s: &str) -> IResult<&str, TableFieldType<'_>> {
    // Try parsing a type as an identifier_path.
    // All primitive types fit but type resolution will happen later.
    let non_vector = preceded(multispace0, identifier_path);
    // let open = tuple((char('['),));
    // let close = tuple((multispace0, char(']')));
    let open = char('[');
    let close = preceded(multispace0, char(']'));
    let vector = delimited(open, preceded(multispace0, identifier_path), close);
    alt((
        map(vector, |ident_path| TableFieldType {
            ident_path,
            is_vector: true,
        }),
        map(non_vector, |ident_path| TableFieldType {
            ident_path,
            is_vector: false,
        }),
    ))(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TableField<'s> {
    field_name: &'s str,
    field_type: TableFieldType<'s>,
    documentation: Documentation<'s>,
    default_value: Option<&'s str>,
    metadata: Vec<KeyVal<'s>>,
}

fn table_field(schema: &str) -> IResult<&str, TableField<'_>> {
    let (rest, (documentation, field_name, _, field_type, default_value, metadata, _)) =
        tuple((
            skip_ws(documentation_block),
            skip_ws(identifier),
            skip_ws(char(':')),
            skip_ws(field_type),
            maybe_eq_value,
            skip_ws(maybe_keyvals),
            skip_ws(char(';')),
        ))(schema)?;
    Ok((
        rest,
        TableField {
            documentation,
            field_name,
            default_value,
            field_type,
            metadata,
        },
    ))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Table<'s> {
    pub name: &'s str,
    fields: Vec<TableField<'s>>,
    metadata: Vec<KeyVal<'s>>,
    documentation: Documentation<'s>,
    is_struct: bool,
}

fn table_declaration(schema: &str) -> IResult<&str, Table<'_>> {
    let struct_or_table = map(alt((tag("struct"), tag("table"))), |t| t == "struct");
    let table_fields = delimited_by_chars('{', many0(table_field), '}');
    map(
        tuple((
            skip_ws(documentation_block),
            skip_ws(struct_or_table),
            skip_ws(identifier),
            skip_ws(maybe_keyvals),
            table_fields,
        )),
        |(documentation, is_struct, name, metadata, fields)| Table {
            name,
            fields,
            metadata,
            documentation,
            is_struct,
        },
    )(schema)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumVariant<'s> {
    pub name: &'s str,
    documentation: Documentation<'s>,
    value: Option<&'s str>, // should be integer.
}
fn enum_variant(schema: &str) -> IResult<&str, EnumVariant<'_>> {
    map(
        tuple((
            skip_ws(documentation_block),
            skip_ws(identifier),
            maybe_eq_value,
        )),
        |(documentation, name, value)| EnumVariant {
            documentation,
            name,
            value,
        },
    )(schema)
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Enum<'s> {
    pub name: &'s str,
    enum_type: &'s str,
    is_union: bool,
    documentation: Documentation<'s>,
    variants: Vec<EnumVariant<'s>>,
}

fn enum_body(schema: &str) -> IResult<&str, Vec<EnumVariant<'_>>> {
    let variant_list = separated_list(skip_ws(char(',')), enum_variant);
    let maybe_trailing_comma = terminated(variant_list, opt(skip_ws(char(','))));
    delimited_by_chars('{', maybe_trailing_comma, '}')(schema)
}

fn enum_declaration(schema: &str) -> IResult<&str, Enum<'_>> {
    map(
        tuple((
            skip_ws(documentation_block),
            preceded(skip_ws(tag("enum")), skip_ws(identifier)),
            preceded(skip_ws(char(':')), skip_ws(identifier)),
            skip_ws(enum_body),
        )),
        |(documentation, name, enum_type, variants)| Enum {
            documentation,
            name,
            enum_type,
            is_union: true,
            variants,
        },
    )(schema)
}
fn union_declaration(schema: &str) -> IResult<&str, Enum<'_>> {
    map(
        tuple((
            skip_ws(documentation_block),
            // CASPER: Unions may have type names!!!
            preceded(skip_ws(tag("union")), skip_ws(identifier)),
            skip_ws(enum_body),
        )),
        |(documentation, name, variants)| Enum {
            name,
            documentation,
            enum_type: "",
            is_union: true,
            variants,
        },
    )(schema)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RpcMethod<'s> {
    pub name: &'s str,
    documentation: Documentation<'s>,
    argument_type: IdentifierPath<'s>,
    return_type: IdentifierPath<'s>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RpcService<'s> {
    pub name: &'s str,
    documentation: Documentation<'s>,
    methods: Vec<RpcMethod<'s>>,
}
fn rpc_method(schema: &str) -> IResult<&str, RpcMethod<'_>> {
    map(
        tuple((
            skip_ws(documentation_block),
            skip_ws(identifier),
            delimited_by_chars('(', identifier_path, ')'),
            delimited_by_chars(':', identifier_path, ';'),
        )),
        |(documentation, name, argument_type, return_type)| RpcMethod {
            name,
            documentation,
            argument_type,
            return_type,
        },
    )(schema)
}

fn rpc_service_declaration(schema: &str) -> IResult<&str, RpcService<'_>> {
    let rpc_service = map(
        tuple((
            skip_ws(documentation_block),
            preceded(skip_ws(tag("rpc_service")), skip_ws(identifier)),
            delimited_by_chars('{', many0(rpc_method), '}'),
        )),
        |(documentation, name, methods)| RpcService {
            name,
            documentation,
            methods,
        },
    );

    rpc_service(schema)
}

fn simple_declaration<'s, T>(
    keyword: &'static str,
    parser: impl Fn(&'s str) -> IResult<&'s str, T>,
) -> impl Fn(&'s str) -> IResult<&'s str, T> {
    delimited(skip_ws(tag(keyword)), skip_ws(parser), skip_ws(char(';')))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration<'s> {
    Table(Table<'s>),
    Enum(Enum<'s>),
    RpcService(RpcService<'s>),
    Namespace(IdentifierPath<'s>),
    FileExtension(&'s str),
    FileIdentifier(&'s str),
}
fn declaration(schema: &str) -> IResult<&str, Declaration<'_>> {
    let namespace_declaration = simple_declaration("namespace", identifier_path);
    let file_ext_declaration = simple_declaration("file_extension", string_literal);
    let file_id_declaration = simple_declaration("file_identifier", string_literal);
    alt((
        map(table_declaration, Declaration::Table),
        map(enum_declaration, Declaration::Enum),
        map(union_declaration, Declaration::Enum),
        map(rpc_service_declaration, Declaration::RpcService),
        map(namespace_declaration, Declaration::Namespace),
        map(file_ext_declaration, Declaration::FileExtension),
        map(file_id_declaration, Declaration::FileIdentifier),
    ))(schema)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Schema<'s> {
    pub included_files: Vec<&'s str>,
    pub declarations: Vec<Declaration<'s>>,
}
pub fn schema(s: &str) -> IResult<&str, Schema<'_>> {
    let include_decl = simple_declaration("include", string_literal);
    map(
        tuple((
            many0(skip_ws(include_decl)),
            many0(skip_ws(declaration)),
            skip_ws(multispace0),
        )),
        |(included_files, declarations, _)| Schema {
            included_files,
            declarations,
        },
    )(s)
}
