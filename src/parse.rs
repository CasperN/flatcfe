use nom;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while};
use nom::character::complete::{alpha1, char, digit1, multispace0, multispace1, none_of, one_of};
use nom::combinator::{map, opt, peek, recognize, value};
use nom::multi::{many0, separated_nonempty_list, separated_list};
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use nom_locate::LocatedSpan;

#[cfg(test)]
#[path = "./parse_test.rs"]
mod test;

type Span<'s> = LocatedSpan<&'s str>;
pub type ParseResult<'s, T> = nom::IResult<Span<'s>, T>;

/// Drop location information. We only need them for declarations and field/variant declarations.
fn deloc<'s, T: nom::AsBytes + Copy>(
    parser: impl Fn(Span<'s>) -> ParseResult<'s, LocatedSpan<T>>,
) -> impl Fn(Span<'s>) -> ParseResult<T> {
    map(parser, |x| *x.fragment())
}

// Skips multispace0 and comments.
fn skip_ws<'s, T>(
    parser: impl Fn(Span<'s>) -> ParseResult<'s, T>,
) -> impl Fn(Span<'s>) -> ParseResult<'s, T> {
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

fn string_literal<'s>(s: Span<'s>) -> ParseResult<&str> {
    let escaped_string = nom::bytes::complete::escaped(is_not("\"\\"), '\\', one_of("\"\\"));
    delimited(skip_ws(char('"')), deloc(escaped_string), char('"'))(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Documentation<'s>(pub Vec<&'s str>);
fn documentation_block<'s>(s: Span<'s>) -> ParseResult<Documentation> {
    let doc = deloc(preceded(skip_ws(tag("///")), is_not("\n\r")));
    map(many0(doc), Documentation)(s)
}

fn identifier<'s>(s: Span<'s>) -> ParseResult<LocatedSpan<&str>> {
    let head = alt((alpha1, tag("_")));
    let body = take_while(|c: char| c.is_ascii_alphanumeric() || c == '_');
    preceded(peek(head), body)(s)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct IdentifierPath<'s>(pub &'s str);
fn identifier_path<'s>(s: Span<'s>) -> ParseResult<IdentifierPath> {
    map(
        recognize(separated_nonempty_list(
            nom::character::complete::char('.'),
            identifier,
        )),
        |p| IdentifierPath(*p.fragment()),
    )(s)
}

fn delimited_by_chars<'s, T>(
    left: char,
    parser: impl Fn(Span<'s>) -> ParseResult<'s, T>,
    right: char,
) -> impl Fn(Span<'s>) -> ParseResult<'s, T> {
    delimited(skip_ws(char(left)), skip_ws(parser), skip_ws(char(right)))
}

// TODO: parse single value. (value is not an identifier)
fn maybe_eq_value<'s>(s: Span<'s>) -> ParseResult<Option<&str>> {
    opt(deloc(preceded(skip_ws(char('=')), skip_ws(identifier))))(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct KeyVal<'s> {
    pub key: &'s str,
    pub value: Option<&'s str>,
}
fn keyval(schema: Span<'_>) -> ParseResult<KeyVal<'_>> {
    let (rest, key) = deloc(preceded(multispace0, identifier))(schema)?;
    let (rest, value) = maybe_eq_value(rest)?;
    Ok((rest, KeyVal { key, value }))
}
// Returns an empty list if there's no list.
fn maybe_keyvals(schema: Span<'_>) -> ParseResult<Vec<KeyVal<'_>>> {
    let kv_list = separated_list(skip_ws(char(',')), keyval);
    map(
        opt(delimited_by_chars('(', kv_list, ')')),
        |v| v.unwrap_or_default(),
    )(schema)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type<'s> {
    Single(IdentifierPath<'s>),
    Vector(IdentifierPath<'s>),
    Array(IdentifierPath<'s>, u16),
}
fn field_type<'s>(s: Span<'s>) -> ParseResult<Type<'_>> {
    let array = delimited_by_chars(
        '[',
        separated_pair(
            skip_ws(identifier_path),
            skip_ws(char(':')),
            skip_ws(digit1),
        ),
        ']',
    );
    dbg!(alt((
        map(skip_ws(identifier_path), |id| Type::Single(id)),
        map(delimited_by_chars('[', identifier_path, ']'), |id| {
            Type::Vector(id)
        }),
        map(array, |(id, cardinality)| {
            Type::Array(id, cardinality.parse::<u16>().unwrap())
        }),
    ))(s))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Metadata<'s> {
    pub documentation: Documentation<'s>,
    pub line: u32,
    pub column: u32,
    pub attributes: Vec<KeyVal<'s>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TableField<'s> {
    pub field_name: &'s str,
    pub field_type: Type<'s>,
    pub metadata: Metadata<'s>,
    pub default_value: Option<&'s str>,
}

fn table_field(schema: Span<'_>) -> ParseResult<TableField<'_>> {
    let (rest, (documentation, field_name, _, field_type, default_value, attributes, _)) =
        tuple((
            skip_ws(documentation_block),
            skip_ws(identifier),
            skip_ws(char(':')),
            skip_ws(field_type),
            maybe_eq_value,
            skip_ws(maybe_keyvals),
            skip_ws(char(';')),
        ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: field_name.location_line(),
        column: field_name.get_column() as u32,
        attributes,
    };
    Ok((
        rest,
        TableField {
            field_name: *field_name.fragment(),
            metadata,
            default_value,
            field_type,
        },
    ))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Table<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    pub fields: Vec<TableField<'s>>,
    pub is_struct: bool,
}

fn table_declaration(schema: Span<'_>) -> ParseResult<Table<'_>> {
    let struct_or_table = map(
        alt((tag("struct"), tag("table"))),
        |t: LocatedSpan<&str>| *t.fragment() == "struct",
    );
    let table_fields = delimited_by_chars('{', many0(table_field), '}');
    let (rest, (documentation, is_struct, name, attributes, fields)) = tuple((
        skip_ws(documentation_block),
        skip_ws(struct_or_table),
        skip_ws(identifier),
        skip_ws(maybe_keyvals),
        table_fields,
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes,
    };
    Ok((
        rest,
        Table {
            name: *name.fragment(),
            metadata,
            fields,
            is_struct,
        },
    ))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumVariant<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    // For unions
    pub union_type: Option<Type<'s>>,
    pub value: Option<&'s str>, // should be integer.
}
fn enum_variant(schema: Span<'_>) -> ParseResult<EnumVariant<'_>> {
    let (rest, (documentation, name, union_type, value)) = tuple((
        skip_ws(documentation_block),
        skip_ws(identifier),
        opt(preceded(skip_ws(char(':')), skip_ws(field_type))),
        maybe_eq_value,
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes: Vec::new(), // Not allowed so far.
    };
    Ok((
        rest,
        EnumVariant {
            name: *name.fragment(),
            metadata,
            union_type,
            value,
        },
    ))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Enum<'s> {
    pub name: &'s str,
    pub enum_type: &'s str,
    pub is_union: bool,
    pub metadata: Metadata<'s>,
    pub variants: Vec<EnumVariant<'s>>,
}

fn enum_body(schema: Span<'_>) -> ParseResult<Vec<EnumVariant<'_>>> {
    let variant_list = separated_list(skip_ws(char(',')), enum_variant);
    let maybe_trailing_comma = terminated(variant_list, opt(skip_ws(char(','))));
    delimited_by_chars('{', maybe_trailing_comma, '}')(schema)
}

fn enum_declaration(schema: Span<'_>) -> ParseResult<Enum<'_>> {
    let (rest, (documentation, name, enum_type, variants)) = tuple((
        skip_ws(documentation_block),
        preceded(skip_ws(tag("enum")), skip_ws(identifier)),
        preceded(skip_ws(char(':')), skip_ws(identifier)),
        skip_ws(enum_body),
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes: Vec::new(), // not allowed so far.
    };
    Ok((
        rest,
        Enum {
            name: *name.fragment(),
            metadata,
            enum_type: *enum_type.fragment(),
            is_union: false,
            variants,
        },
    ))
}

fn union_declaration(schema: Span<'_>) -> ParseResult<Enum<'_>> {
    let (rest, (documentation, name, variants)) = tuple((
        skip_ws(documentation_block),
        // CASPER: Unions may have type names!!!
        preceded(skip_ws(tag("union")), skip_ws(identifier)),
        skip_ws(enum_body),
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes: Vec::new(),
    };
    Ok((
        rest,
        Enum {
            name: *name.fragment(),
            metadata,
            enum_type: "",
            is_union: true,
            variants,
        },
    ))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RpcMethod<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    pub argument_type: IdentifierPath<'s>,
    pub return_type: IdentifierPath<'s>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RpcService<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    pub methods: Vec<RpcMethod<'s>>,
}
fn rpc_method(schema: Span<'_>) -> ParseResult<RpcMethod<'_>> {
    let (rest, (documentation, name, argument_type, return_type)) = tuple((
        skip_ws(documentation_block),
        skip_ws(identifier),
        delimited_by_chars('(', identifier_path, ')'),
        delimited_by_chars(':', identifier_path, ';'),
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes: Vec::new(),
    };
    Ok((
        rest,
        RpcMethod {
            name: *name.fragment(),
            metadata,
            argument_type,
            return_type,
        },
    ))
}

fn rpc_service_declaration(schema: Span<'_>) -> ParseResult<RpcService<'_>> {
    let (rest, (documentation, name, methods)) = tuple((
        skip_ws(documentation_block),
        preceded(skip_ws(tag("rpc_service")), skip_ws(identifier)),
        delimited_by_chars('{', many0(rpc_method), '}'),
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes: Vec::new(),
    };
    Ok((
        rest,
        RpcService {
            name: *name.fragment(),
            metadata,
            methods,
        },
    ))
}

fn simple_declaration<'s, T>(
    keyword: &'static str,
    parser: impl Fn(Span<'s>) -> ParseResult<'s, T>,
) -> impl Fn(Span<'s>) -> ParseResult<'s, T> {
    delimited(skip_ws(tag(keyword)), skip_ws(parser), skip_ws(char(';')))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Detail<'s> {
    Struct {
        is_struct: bool,
        fields: Vec<TableField<'s>>,
    },
    Enum {
        is_union: bool,
        enum_type: &'s str,
        variants: Vec<EnumVariant<'s>>,
    },
    RpcService {
        methods: Vec<RpcMethod<'s>>,
    },
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Symbol<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    pub detail: Detail<'s>,
}
impl<'s> Symbol<'s> {
    fn from_table(table: Table<'s>) -> Self {
        let Table {
            name,
            metadata,
            fields,
            is_struct,
        } = table;
        Self {
            name,
            metadata,
            detail: Detail::Struct { is_struct, fields },
        }
    }
    fn from_enum(e: Enum<'s>) -> Self {
        let Enum {
            name,
            enum_type,
            metadata,
            variants,
            is_union,
        } = e;
        Self {
            name,
            metadata,
            detail: Detail::Enum {
                is_union,
                enum_type,
                variants,
            },
        }
    }
    fn from_rpc_service(r: RpcService<'s>) -> Self {
        let RpcService {
            name,
            metadata,
            methods,
        } = r;
        Self {
            name,
            metadata,
            detail: Detail::RpcService { methods },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration<'s> {
    Symbol(Symbol<'s>),
    Namespace(IdentifierPath<'s>),
    FileExtension(&'s str),
    FileIdentifier(&'s str),
}
fn declaration(schema: Span<'_>) -> ParseResult<Declaration<'_>> {
    let namespace_declaration = simple_declaration("namespace", identifier_path);
    let file_ext_declaration = simple_declaration("file_extension", string_literal);
    let file_id_declaration = simple_declaration("file_identifier", string_literal);
    alt((
        map(table_declaration, |t| {
            Declaration::Symbol(Symbol::from_table(t))
        }),
        map(enum_declaration, |e| {
            Declaration::Symbol(Symbol::from_enum(e))
        }),
        map(union_declaration, |e| {
            Declaration::Symbol(Symbol::from_enum(e))
        }),
        map(rpc_service_declaration, |r| {
            Declaration::Symbol(Symbol::from_rpc_service(r))
        }),
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
fn schema<'s>(s: Span<'s>) -> ParseResult<Schema<'_>> {
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

fn start<'s, T>(
    parser: impl Fn(Span<'s>) -> ParseResult<'s, T>,
) -> impl Fn(&'s str) -> ParseResult<'s, T> {
    move |s: &'s str| parser(LocatedSpan::new(s))
}

pub fn fully_parse_schema<'s>(contents: &'s str) -> Schema<'s> {
    let (remainder, schema) = start(schema)(contents).expect("Parse Error.");
    assert_eq!(*remainder.fragment(), "", "Did not fully parse schema.");
    schema
}
