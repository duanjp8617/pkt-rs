use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::RandomState;

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
}

impl MessageGroupName {
    pub fn new(name: String, msg_names: Vec<String>) -> Self {
        Self { name, msg_names }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn msg_names(&self) -> &Vec<String> {
        &self.msg_names
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
    items: &'a [&'a (ParsedItem, (usize, usize))],
    msg_groups: HashMap<&'a str, Vec<&'a Message>>,
}

impl<'a> TopLevel<'a> {
    pub fn new(
        parsed_items: &'a [&(ParsedItem, (usize, usize))],
    ) -> Result<Self, (Error, (usize, usize))> {
        let mut all_names = HashSet::new();
        let mut all_msgs = HashMap::new();
        let mut msg_groups = Vec::new();

        for (parsed_item, span) in parsed_items.iter() {
            let name = match parsed_item {
                ParsedItem::Packet_(p) => p.protocol_name(),
                ParsedItem::Message_(m) => {
                    all_msgs.insert(m.protocol_name(), m);
                    m.protocol_name()
                }
                ParsedItem::MessageGroupName_(mg) => {
                    msg_groups.push((mg, span));
                    mg.name()
                }
            };
            if all_names.contains(name) {
                return_err!((
                    Error::top_level(
                        1,
                        format!("duplicated packet/message/(message group) name {}", name)
                    ),
                    *span
                ))
            }
            all_names.insert(name);
        }

        let mut resulting_map = HashMap::new();
        for (mg, span) in msg_groups {
            let msgs = Self::check_msg_group(mg, &all_msgs).map_err(|err| (err, *span))?;
            resulting_map.insert(mg.name(), msgs);
        }

        Ok(Self {
            items: parsed_items,
            msg_groups: resulting_map,
        })
    }

    pub fn item_iter(&self) -> impl Iterator<Item = &'a ParsedItem> {
        self.items.iter().map(|t| &t.0)
    }

    pub fn msg_group(&self, msg_group_name: &'a str) -> Option<&Vec<&'a Message>> {
        self.msg_groups.get(msg_group_name)
    }

    fn check_msg_group(
        mg: &MessageGroupName,
        msgs: &HashMap<&'a str, &'a Message>,
    ) -> Result<Vec<&'a Message>, Error> {
        let mut names_iter = mg.msg_names.iter();

        let Some(first_msg_name) = names_iter.next() else {
            panic!()
        };
        let first_msg = msgs.get(&(*first_msg_name)[..]).ok_or(Error::top_level(
            3,
            format!("message {first_msg_name} is not defined"),
        ))?;
        let first_cond = first_msg.cond().as_ref().ok_or(Error::top_level(
            4,
            format!("cond of message {first_msg_name} is not defined"),
        ))?;

        let mut name_dedup = HashSet::new();
        name_dedup.insert(&(*first_msg_name)[..]);
        let (target_field, target_pos) = first_msg.header().field(first_cond.field_name()).unwrap();
        let mut result_vec = vec![*first_msg];
        let mut compared_values_dedup: HashSet<u64, RandomState> =
            HashSet::from_iter(first_cond.compared_values().iter().map(|val| *val));

        for name in names_iter {
            // 1. the names of the `mg` should not be duplicated.
            if name_dedup.contains(&(*name)[..]) {
                return_err!(Error::top_level(2, format!("message {name} appears twice")))
            }
            name_dedup.insert(&(*name)[..]);

            // 2. Each message name contained in the message group should be defined.
            let subsequent_msg = msgs.get(&(*name)[..]).ok_or(Error::top_level(
                3,
                format!("message {first_msg_name} is not defined"),
            ))?;

            // 3. Each message should has a valid cond.
            let subsequent_cond = subsequent_msg.cond().as_ref().ok_or(Error::top_level(
                4,
                format!("cond of message {name} is not defined"),
            ))?;

            // 4. the position, bit size, repr of the cond field should be the same
            let (cond_field, cond_pos) = subsequent_msg
                .header()
                .field(subsequent_cond.field_name())
                .unwrap();
            if (cond_field.bit != target_field.bit)
                || (cond_pos != target_pos)
                || (cond_field.repr != target_field.repr)
            {
                return_err!(
                    Error::top_level(5, format!("the cond field of message {name} is not the same as that of message {first_msg_name}")))
            }

            // 5. the compared value should not be the same.
            for compared_value in subsequent_cond.compared_values() {
                if compared_values_dedup.contains(compared_value) {
                    return_err!(Error::top_level(6, format!("message {name} appears twice")))
                }
                compared_values_dedup.insert(*compared_value);
            }

            result_vec.push(*subsequent_msg);
        }

        Ok(result_vec)
    }
}
