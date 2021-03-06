use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while};
use nom::character::complete::{alpha1, char, digit1, multispace0, multispace1, none_of, one_of};
use nom::combinator::{map, opt, peek, recognize, value};
use nom::multi::{many0, separated_list, separated_nonempty_list};
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use nom_locate::LocatedSpan;

#[cfg(test)]
#[path = "./parse_test.rs"]
mod test;

type Span<'s> = LocatedSpan<&'s str>;
pub type ParseResult<'s, T> = nom::IResult<Span<'s>, T>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct IdentifierPath<'s>(pub &'s str);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type<'s> {
    Single(IdentifierPath<'s>),
    Vector(IdentifierPath<'s>),
    Array(IdentifierPath<'s>, u16),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct KeyVal<'s> {
    pub key: &'s str,
    pub value: Option<&'s str>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Documentation<'s>(pub Vec<&'s str>);

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumVariant<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    // For unions
    pub union_type: Option<Type<'s>>,
    pub value: Option<&'s str>, // should be integer.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RpcMethod<'s> {
    pub name: &'s str,
    pub metadata: Metadata<'s>,
    pub argument_type: IdentifierPath<'s>,
    pub return_type: IdentifierPath<'s>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration<'s> {
    Symbol(Symbol<'s>),
    Namespace(IdentifierPath<'s>),
    FileExtension(&'s str),
    FileIdentifier(&'s str),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Schema<'s> {
    pub included_files: Vec<&'s str>,
    pub declarations: Vec<Declaration<'s>>,
}

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

fn string_literal(s: Span) -> ParseResult<&str> {
    let escaped_string = nom::bytes::complete::escaped(is_not("\"\\"), '\\', one_of("\"\\"));
    delimited(skip_ws(char('"')), deloc(escaped_string), char('"'))(s)
}

fn documentation_block(s: Span) -> ParseResult<Documentation> {
    let doc = deloc(preceded(skip_ws(tag("///")), is_not("\n\r")));
    map(many0(doc), Documentation)(s)
}

fn identifier(s: Span) -> ParseResult<LocatedSpan<&str>> {
    let head = alt((alpha1, tag("_")));
    let body = take_while(|c: char| c.is_ascii_alphanumeric() || c == '_');
    preceded(peek(head), body)(s)
}

fn identifier_path(s: Span) -> ParseResult<IdentifierPath> {
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
fn maybe_eq_value(s: Span) -> ParseResult<Option<&str>> {
    opt(deloc(preceded(skip_ws(char('=')), skip_ws(identifier))))(s)
}

fn keyval(schema: Span) -> ParseResult<KeyVal> {
    let (rest, key) = deloc(preceded(multispace0, identifier))(schema)?;
    let (rest, value) = maybe_eq_value(rest)?;
    Ok((rest, KeyVal { key, value }))
}
// Returns an empty list if there's no list.
fn maybe_keyvals(schema: Span) -> ParseResult<Vec<KeyVal>> {
    let kv_list = separated_list(skip_ws(char(',')), keyval);
    map(opt(delimited_by_chars('(', kv_list, ')')), |v| {
        v.unwrap_or_default()
    })(schema)
}

fn field_type(s: Span) -> ParseResult<Type> {
    let array = delimited_by_chars(
        '[',
        separated_pair(
            skip_ws(identifier_path),
            skip_ws(char(':')),
            skip_ws(digit1),
        ),
        ']',
    );
    alt((
        map(skip_ws(identifier_path), Type::Single),
        map(delimited_by_chars('[', identifier_path, ']'), Type::Vector),
        map(array, |(id, cardinality)| {
            Type::Array(id, cardinality.parse::<u16>().unwrap())
        }),
    ))(s)
}

fn table_field(schema: Span) -> ParseResult<TableField> {
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

fn table_declaration(schema: Span) -> ParseResult<Declaration> {
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
        Declaration::Symbol(Symbol {
            name: name.fragment(),
            metadata,
            detail: Detail::Struct { is_struct, fields },
        }),
    ))
}

fn enum_variant(schema: Span) -> ParseResult<EnumVariant> {
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

fn enum_body(schema: Span) -> ParseResult<Vec<EnumVariant>> {
    let variant_list = separated_list(skip_ws(char(',')), enum_variant);
    let maybe_trailing_comma = terminated(variant_list, opt(skip_ws(char(','))));
    delimited_by_chars('{', maybe_trailing_comma, '}')(schema)
}

fn enum_declaration(schema: Span) -> ParseResult<Declaration> {
    let (rest, (documentation, name, enum_type, attributes, variants)) = tuple((
        skip_ws(documentation_block),
        preceded(skip_ws(tag("enum")), skip_ws(identifier)),
        preceded(skip_ws(char(':')), skip_ws(identifier)),
        maybe_keyvals,
        skip_ws(enum_body),
    ))(schema)?;
    let metadata = Metadata {
        documentation,
        line: name.location_line(),
        column: name.get_column() as u32,
        attributes,
    };
    Ok((
        rest,
        Declaration::Symbol(Symbol {
            name: name.fragment(),
            metadata,
            detail: Detail::Enum {
                is_union: false,
                enum_type: enum_type.fragment(),
                variants,
            },
        }),
    ))
}

fn union_declaration(schema: Span) -> ParseResult<Declaration> {
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
        Declaration::Symbol(Symbol {
            name: name.fragment(),
            metadata,
            detail: Detail::Enum {
                variants,
                enum_type: "",
                is_union: true,
            },
        }),
    ))
}

fn rpc_method(schema: Span) -> ParseResult<RpcMethod> {
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

fn rpc_service_declaration(schema: Span) -> ParseResult<Declaration> {
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
        Declaration::Symbol(Symbol {
            name: name.fragment(),
            metadata,
            detail: Detail::RpcService { methods },
        }),
    ))
}

fn simple_declaration<'s, T>(
    keyword: &'static str,
    parser: impl Fn(Span<'s>) -> ParseResult<'s, T>,
) -> impl Fn(Span<'s>) -> ParseResult<'s, T> {
    delimited(skip_ws(tag(keyword)), skip_ws(parser), skip_ws(char(';')))
}

fn declaration(schema: Span) -> ParseResult<Declaration> {
    let namespace_declaration = simple_declaration("namespace", identifier_path);
    let file_ext_declaration = simple_declaration("file_extension", string_literal);
    let file_id_declaration = simple_declaration("file_identifier", string_literal);
    alt((
        table_declaration,
        enum_declaration,
        union_declaration,
        rpc_service_declaration,
        map(namespace_declaration, Declaration::Namespace),
        map(file_ext_declaration, Declaration::FileExtension),
        map(file_id_declaration, Declaration::FileIdentifier),
    ))(schema)
}

fn schema(s: Span) -> ParseResult<Schema> {
    let include_decl = simple_declaration("include", string_literal);
    map(
        tuple((
            many0(include_decl),
            many0(declaration),
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

pub fn fully_parse_schema(contents: &str) -> Schema {
    let (remainder, schema) = start(schema)(contents).expect("Parse Error.");
    assert_eq!(*remainder.fragment(), "", "Did not fully parse schema.");
    schema
}
