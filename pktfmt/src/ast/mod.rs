use std::fmt;

mod number;
pub use number::*;

mod field;
pub use field::*;

mod header;
pub use header::*;

mod length;
pub use length::*;

/// The top level ast type for the `packet`` definition
#[derive(Debug)]
pub struct Packet {
    protocol_name: String,
    header: Header,
    length: Length,
}

impl Packet {
    pub fn new(protocol_name: &str, header: header::Header, length: length::Length) -> Self {
        Self {
            protocol_name: protocol_name.to_string(),
            header,
            length,
        }
    }

    pub fn protocol_name(&self) -> &str {
        &self.protocol_name
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn length(&self) -> &Length {
        &self.length
    }
}

// An enum type to describe the type of the error
#[derive(Debug, Clone, PartialEq, Eq)]
enum ErrorType {
    // 1 errors
    NumberError,
    // 6 errors
    FieldDef,
    // 5 errors
    HeaderDef,
    // 10 errors
    LengthDef,
}

// Record the error type and error index.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ErrorPos(ErrorType, usize);

impl fmt::Display for ErrorPos {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            ErrorType::NumberError => write!(fmt, "number error {}", self.1),
            ErrorType::FieldDef => write!(fmt, "field error {}", self.1),
            ErrorType::HeaderDef => write!(fmt, "header error {}", self.1),
            ErrorType::LengthDef => write!(fmt, "length error {}", self.1),
        }
    }
}

/// The ast-related error when parsing the pktfmt script.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Error {
    pos: ErrorPos,
    reason: String,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:\n{}", self.pos, self.reason)
    }
}

impl Error {
    pub fn num_error(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::NumberError, index),
            reason,
        }
    }

    pub fn field(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::FieldDef, index),
            reason,
        }
    }

    pub fn header(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::HeaderDef, index),
            reason,
        }
    }

    pub fn length(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::LengthDef, index),
            reason,
        }
    }
}
