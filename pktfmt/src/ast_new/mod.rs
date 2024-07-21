use std::collections::HashMap;
use std::fmt;
use std::io::{Read, Write};

use crate::utils::Spanned;

mod field;
pub use field::*;

mod header;
pub use header::*;

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
