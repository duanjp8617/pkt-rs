use std::collections::HashMap;
use std::fmt;
use std::io::{Read, Write};

use crate::utils::Spanned;

mod field;
pub use field::*;

mod header;
pub use header::*;

mod length;
pub use length::*;

/// The top level ast type for the packet definition
#[derive(Debug, Clone)]
pub struct Packet {
    pub protocol_name: String,
    pub field_list: Vec<(String, Field)>,
    pub field_position: HashMap<String, (BitPos, usize)>,
    pub length_list: Vec<LengthField>,
}

// impl Packet {
//     /// We should perform checks to ensure that the length fields
//     /// are correctly defined, but for now, we ignore it.
//     pub fn new(
//         protocol_name: String,
//         field_list: Vec<(String, Field)>,
//         field_pos_map: HashMap<String, (BitPos, usize)>,
//         header_len: Option<(Option<Box<AlgExpr>>, String, (usize, usize))>,
//         payload_len: Option<(Option<Box<AlgExpr>>, (usize, usize))>,
//         packet_len: Option<(Option<Box<AlgExpr>>, (usize, usize))>,
//     ) -> Result<Self, (Error, (usize, usize))> {
//         // TODO:
//         // 1. make sure that the field used for length computation
//         // is not generated, its `repr` is the same as `arg` and `repr`
//         // is not `ByteSlice`.
//         // 2. make sure that the identifier contained in the length expression
//         // corresponds to an existing field name
//         // 3. make sure that when the field is the max value,
//         // the length expression computation does not overflow
//         // 4. the max value of the length field must be smaller than a pre-defined
//         // constant!
//         let mut length_fields = vec![LengthField::None, LengthField::None, LengthField::None];
//         let mut option_name = None;

//         header_len.map(|(alg_expr, name, _)| {
//             length_fields[HEADER_LEN_IDX] =
//                 LengthField::Expr(alg_expr.and_then(|expr| expr.try_take_usable_expr()));

//             option_name = Some(name);
//         });
//         payload_len.map(|(alg_expr, _)| {
//             length_fields[PAYLOAD_LEN_IDX] =
//                 LengthField::Expr(alg_expr.and_then(|expr| expr.try_take_usable_expr()));
//         });
//         packet_len.map(|(alg_expr, _)| {
//             length_fields[PACKET_LEN_IDX] =
//                 LengthField::Expr(alg_expr.and_then(|expr| expr.try_take_usable_expr()));
//         });

//         Ok(Self {
//             protocol_name,
//             field_list,
//             field_pos_map,
//             length_fields,
//             option_name,
//         })
//     }
// }

/// An enum type to describe where the error is generated.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorPos {
    FieldDef,
    HeaderDef,
}

impl fmt::Display for ErrorPos {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FieldDef => write!(fmt, "invalid Field definition"),
            Self::HeaderDef => write!(fmt, "invalid Header definition"),
        }
    }
}

/// The ast-related error when parsing the pktfmt script.
#[derive(Debug, Clone)]
pub struct Error {
    pos: ErrorPos,
    reason: String,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{:?}:\n{}", self.pos, self.reason)
    }
}

impl Error {
    /// Return an field definition error.
    pub fn field(reason: String) -> Self {
        Self {
            pos: ErrorPos::FieldDef,
            reason,
        }
    }

    /// Return an header definition error.
    pub fn header(reason: String) -> Self {
        Self {
            pos: ErrorPos::FieldDef,
            reason,
        }
    }
}
