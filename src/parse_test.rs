use super::*;

fn full_parse<'s, T>(parser: impl Fn(Span<'s>) -> ParseResult<'s, T>, s: &'s str) -> T {
    let (rest, t) = parser(LocatedSpan::new(s)).unwrap();
    assert_eq!(rest.fragment(), &"");
    t
}

#[test]
fn test_skip_ws() {
    fn go<'s>(tag_literal: &str, input: &'s str) {
        let (_, t) = skip_ws(tag(tag_literal))(LocatedSpan::new(input)).unwrap();
        assert_eq!(tag_literal, *t.fragment());
    }
    go("nothing", "nothing");
    go("white", "\n\r\t   white");
    go("space", "         space");
    go("comm", "//       \ncomm");
    go("block", "/*     */block");
    go("all", "//...\n/*     */  \n\r all");
}

#[test]
fn test_string_literal() {
    assert_eq!(start(string_literal)(r#" "hi" "#).unwrap().1, "hi");
    assert_eq!(start(string_literal)(r#" "hi\"" "#).unwrap().1, r#"hi\""#);
    assert_eq!(start(string_literal)(r#" "hi\\ " "#).unwrap().1, r#"hi\\ "#);
    assert_eq!(
        start(string_literal)(r#" "schema.fbs" "#).unwrap().1,
        "schema.fbs"
    );
}

#[test]
fn test_parse_enum_details() {
    let (remainder, e) = start(enum_declaration)(
        "/// Foo documentation\n\
         enum Foo : byte {\n\
             /// its a bar.
             Bar,
             Baz = a1
         }",
    )
    .unwrap();
    assert!(remainder.is_empty());

    assert_eq!(e.name, "Foo");
    assert_eq!(e.metadata.documentation.0, &[" Foo documentation"]);
    assert_eq!(e.enum_type, "byte");
    assert_eq!(e.variants.len(), 2);
    assert_eq!(e.variants[0].name, "Bar");
    assert_eq!(e.variants[0].value, None);
    assert_eq!(e.variants[0].metadata.documentation.0, &[" its a bar."]);
    assert_eq!(e.variants[1].name, "Baz");
    assert_eq!(e.variants[1].value, Some("a1"));
    assert!(e.variants[1].metadata.documentation.0.is_empty());
}

#[test]
fn test_fully_parse_enums() {
    let go = |s| full_parse(enum_declaration, s);
    go("enum Enum: E {}");
    go("enum Enum: E {,}");
    go("enum Enum: E { A }");
    go("enum Enum: E { A, }");
    go("enum Enum: E { A,B }");
    go("enum Enum: E { A, B,}");
    go("///Doc\n  enum /*lol */ Enum  : E { A,\nB,///DocCom\nC,}");
}

#[test]
fn test_parse_table_details() {
    let table = full_parse(
        table_declaration,
        "/// This is my table documentation.\n\
         /// it is on multiple lines.\n\
         table Foo (hello = world){\n\

             /// Sup bro.
            field1:Foo;
           /// Sup
           /// bro
            field2: [Foo.Bar];
            field3 :Foo.Bar.Baz (sup, bro);

            field4 : [Foo.Bar.Baz] (sup = bro);
        }",
    );
    assert_eq!(table.name, "Foo");
    assert_eq!(
        table.metadata.documentation.0,
        &[
            " This is my table documentation.",
            " it is on multiple lines."
        ]
    );
    // Table makes sense.
    assert_eq!(table.is_struct, false);
    assert_eq!(table.metadata.attributes.len(), 1);
    assert_eq!(table.metadata.attributes[0].key, "hello");
    assert_eq!(table.metadata.attributes[0].value, Some("world"));
    assert_eq!(table.fields.len(), 4);
    {
        // Test field1 makes sense.
        let field1 = &table.fields[0];
        assert_eq!(field1.field_name, "field1");
        assert_eq!(field1.metadata.documentation.0, &[" Sup bro."]);
        assert_eq!(&field1.field_type.ident_path.0, &"Foo");
        assert!(!field1.field_type.is_vector);
        assert!(field1.metadata.attributes.is_empty());
    }
    {
        // Test field2 makes sense.
        let field2 = &table.fields[1];
        assert_eq!(field2.field_name, "field2");
        assert_eq!(field2.metadata.documentation.0, &[" Sup", " bro"]);
        assert_eq!(&field2.field_type.ident_path.0, &"Foo.Bar");
        assert!(field2.field_type.is_vector);
        assert!(field2.metadata.attributes.is_empty());
    }
    {
        // Test field3 makes sense.
        let field3 = &table.fields[2];
        assert_eq!(field3.field_name, "field3");
        assert_eq!(&field3.field_type.ident_path.0, &"Foo.Bar.Baz");
        assert!(!field3.field_type.is_vector);
        assert_eq!(field3.metadata.attributes.len(), 2);
        assert_eq!(field3.metadata.attributes[0].key, "sup");
        assert_eq!(field3.metadata.attributes[0].value, None);
        assert_eq!(field3.metadata.attributes[1].key, "bro");
        assert_eq!(field3.metadata.attributes[1].value, None);
    }
    {
        // Test field4 makes sense.
        let field4 = &table.fields[3];
        assert_eq!(field4.field_name, "field4");
        assert_eq!(&field4.field_type.ident_path.0, &"Foo.Bar.Baz");
        assert!(field4.field_type.is_vector);
        assert_eq!(field4.metadata.attributes.len(), 1);
        assert_eq!(field4.metadata.attributes[0].key, "sup");
        assert_eq!(field4.metadata.attributes[0].value, Some("bro"));
    }
}

#[test]
fn test_rpc_service_declaration() {
    full_parse(rpc_method, "\n\r\t/// Foobar \n\tgreet(Hello) : goodbye;");
    full_parse(
        rpc_service_declaration,
        "\n\
        rpc_service Hi {\n\t\r\n\
            greet(Hello) : Goodbye ;
            plead(Please) : ThankYou;
        }",
    );
}

#[test]
fn test_parse_schema() {
    full_parse(
        schema,
        r#"
        include "schema2.rs";

        table Table {
            key: string;
            value: Byte;
        }

        /// e
        enum Enum: byte /**/ { Name, Foop }

        rpc_service Hi {
            greet(Hello) : Goodbye;
        }
        "#,
    );
}
