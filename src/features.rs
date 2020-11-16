use crate::flatc;
use crate::resolve;
use crate::types;

pub fn features(symbols: &[resolve::Symbol]) -> flatc::Features {
    let mut features = flatc::Features::default();
    for s in symbols.iter() {
        match &s.detail {
            resolve::Detail::Struct {
                is_struct: false,
                fields,
            } => {
                if fields.iter().any(|field| {
                    types::is_scalar(field.field_type.base_type) && field.default_value == "null"
                }) {
                    features |= flatc::Features::OptionalScalars;
                }
            }
            resolve::Detail::Struct {
                is_struct: true,
                fields,
            } => {
                if fields
                    .iter()
                    .any(|field| field.field_type.base_type == flatc::BaseType::Array)
                {
                    features |= flatc::Features::FixedSizeArraysInStructs;
                }
            }
            resolve::Detail::Enum {
                is_union: true,
                variants,
                ..
            } => {
                if variants.iter().any(|var| {
                    var.union_type.base_type == flatc::BaseType::String
                        || var.union_type.base_type == flatc::BaseType::Vector
                        || var.union_type.base_type == flatc::BaseType::Struct
                            && !var.union_type.has_references // struct _not table_.
                }) {
                    features |= flatc::Features::AdvancedUnions;
                }
            }
            _ => {}
        }
        if features == flatc::Features::all() {
            break;
        }
    }
    features
}
