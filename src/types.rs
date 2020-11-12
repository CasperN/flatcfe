use crate::flatc;

pub fn resolve_primitive_type(ty: &str) -> Option<flatc::BaseType> {
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

/// If `caller` calling `ty` may resolve to `symbol` return Some(p) where p is the precidence.
/// Assumes `caller` and `symbol` are fully qualified types from the root namespace.
/// The symbol with the lowest precidence is the one that will be selected.
pub fn type_precedence(caller: &str, ty: &str, symbol: &str) -> Option<usize> {
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
