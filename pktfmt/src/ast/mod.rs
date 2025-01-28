use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::utils::render_error;

mod number;
pub use number::*;

mod field;
pub use field::*;

mod header;
pub use header::*;

mod length;
pub use length::*;

mod cond;
pub use cond::*;

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

    pub fn header_template(&self) -> &[u8] {
        self.header.header_template()
    }
}

/// Top level ast type for `message` definition.
///
/// It is basically the same as `Packet`, except that it carries conditional
/// field.
#[derive(Debug)]
pub struct Message {
    protocol_name: String,
    header: Header,
    length: Length,
    cond: Option<Cond>,
}

impl Message {
    pub fn new(
        protocol_name: &str,
        header: header::Header,
        length: length::Length,
        cond: Option<Cond>,
    ) -> Self {
        Self {
            protocol_name: protocol_name.to_string(),
            header,
            length,
            cond,
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

    pub fn cond(&self) -> &Option<Cond> {
        &self.cond
    }

    pub fn header_template(&self) -> &[u8] {
        self.header.header_template()
    }
}

#[derive(Debug)]
pub struct MessageGroupName {
    name: String,
    msg_names: Vec<String>,
    pos: (usize, usize),
}

impl MessageGroupName {
    pub fn new(name: String, msg_names: Vec<String>, pos: (usize, usize)) -> Self {
        Self {
            name,
            msg_names,
            pos,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn msg_names(&self) -> &Vec<String> {
        &self.msg_names
    }

    pub fn pos(&self) -> (usize, usize) {
        self.pos
    }
}

// An enum type to describe the type of the error
#[derive(Debug, Clone, PartialEq, Eq)]
enum ErrorType {
    // 1 errors
    NumberError,
    // 6 errors
    FieldDef,
    // 4 errors
    HeaderDef,
    // 10 errors
    LengthDef,
    // x errors
    CondDef,
    // top-level errors
    TopLevel,
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
            ErrorType::CondDef => write!(fmt, "conditional error {}", self.1),
            ErrorType::TopLevel => write!(fmt, "top level error {}", self.1),
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

    pub fn cond(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::CondDef, index),
            reason,
        }
    }

    pub fn top_level(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::TopLevel, index),
            reason,
        }
    }
}

// calculate the max value of `bit` bits for `u64`
pub(crate) fn max_value(bit: u64) -> Option<u64> {
    assert!(bit > 0);

    if bit > 64 {
        None
    } else if bit < 64 {
        Some((1 << bit) - 1)
    } else {
        Some(u64::MAX)
    }
}

pub enum ParsedItem {
    Packet_(Packet),
    Message_(Message),
    MessageGroupName_(MessageGroupName),
}

pub struct TopLevel<'a> {
    items: &'a [&'a ParsedItem],
    msg_groups: HashMap<&'a str, Vec<&'a Message>>,
}

impl<'a> TopLevel<'a> {
    pub fn msg_groups(&self) -> &HashMap<&'a str, Vec<&'a Message>> {
        &self.msg_groups
    }
}

// impl<'a> TopLevel<'a> {
//     pub fn new(parsed_items: &'a [&ParsedItem]) -> Result<Self, Error> {
//         let mut all_names = HashMap::new();
//         let mut msg_groups = HashMap::new();

//         for (idx, parsed_item) in parsed_items.iter().enumerate() {
//             let name = match parsed_item {
//                 ParsedItem::Packet_(p) => p.protocol_name(),
//                 ParsedItem::Message_(m) => m.protocol_name(),
//                 ParsedItem::MessageGroupName_(mg) => mg.name(),
//             };
//             if all_names.contains_key(name) {
//                 return_err!(Error::top_level(
//                     1,
//                     format!("duplicated packet/message/(message group) name
// {}", name)                 ))
//             }
//             all_names.insert(name, idx);
//             match parsed_item {
//                 ParsedItem::MessageGroupName_(_) => {
//                     msg_groups.insert(name, idx);
//                 }
//                 _ => {}
//             };
//         }

//         let obj = Self {
//             items: parsed_items,
//             all_names,
//             msg_groups,
//         };

//         Ok(obj)
//     }

//     fn check_msg_group(mg: &'a MessageGroup, msgs: HashMap<&'a str, &'a
// Message>) {         // 1. There are no duplicated message names in a message
// group.         // 2. Each message is a valid message that is globally
// defined.         // 3. If the message has a variable header length, then the
// corresponding field for deriving the length must be at the same position.
//         // 4. The message must has a condition, and the field that is used to
// calculate the condition must be at the same position.

//         let mut msg_name_dedup = HashSet::new();
//         for msg_name in mg.msg_names().iter() {

//         }
//     }
// }
