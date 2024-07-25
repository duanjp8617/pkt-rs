use std::collections::HashMap;

use crate::utils::Spanned;

use super::field::Field;
use super::number::MAX_MTU_IN_BYTES;
use super::Error;

const RESERVED_FIELD_NAMES: &[&str] = &["header_len", "payload_len", "packet_len"];

/// An object that represent the fixed-length header defined by the pktfmt
/// script.
///
/// `field_list`: an list that preserves the order of the header fields, with
/// each element being the field name and the field object, used for field
/// object indexing
///
/// `field_position`: a hashmap that maps the field name to the bit position and
/// field list index
#[derive(Debug)]
pub struct Header {
    header_len_in_bytes: usize,
    field_list: Vec<(String, Field)>,
    field_position: HashMap<String, (BitPos, usize)>,
}

impl Header {
    /// Given a parsed header with a list of fields, check its correctness and
    /// construct data structures for efficient header field queries.
    ///
    /// Input arguments:
    ///
    /// `field_list`: a list consists of user-defined header fields.
    ///
    /// `header_pos`: the byte indexes of the header list in the original file
    ///
    /// Return value:
    ///
    /// if succeed: a new `Header` object,
    /// if fail: an error and the file indexes that triggers the error.
    pub fn new(
        field_list: Vec<(Spanned<String>, Field)>,
        header_pos: (usize, usize),
    ) -> Result<Self, (Error, (usize, usize))> {
        // field_name -> (bit position within the header, index of the field list)
        let mut field_position = HashMap::new();

        // temporary variable for recording the field bit position
        let mut global_bit_pos = 0;

        let field_list = field_list
            .into_iter()
            .enumerate()
            .map(|(field_idx, (sp_str, field))| {
                if field_position.get(&sp_str.item).is_some() {
                    // header error 1: duplicated header field name
                    let reason = format!("duplicated header field name {}", &sp_str.item);
                    return_err_1!((Error::header(reason), sp_str.span))
                } else if RESERVED_FIELD_NAMES
                    .iter()
                    .find(|reserved| **reserved == &sp_str.item)
                    .is_some()
                {
                    // header error 2: invalid header field name
                    let reason = format!(
                        "header field name {} is reserved and can't be used",
                        &sp_str.item
                    );
                    return_err_1!((Error::header(reason), sp_str.span))
                } else {
                    // calculate the start and end bit position of the header
                    let start = BitPos::new(global_bit_pos);
                    let end = start.next_pos(field.bit);

                    if field.bit > 8 && start.bit_pos != 0 && end.bit_pos != 7 {
                        // header error 3: mis-aligned header field
                        // If the header field contains multiple bytes, then one of two ends must be
                        // aligned to the byte boudary. In this branch, neither of
                        // the two ends are aligned to the byte boundary, we report an error.
                        let reason = format!(
                            "header field {} is not correctly aligned to the byte boundaries",
                            &sp_str.item
                        );
                        return_err_1!((Error::header(reason), sp_str.span))
                    } else {
                        global_bit_pos += field.bit;
                        field_position.insert(sp_str.item.clone(), (start, field_idx));
                        Ok((sp_str.item, field))
                    }
                }
            })
            .collect::<Result<Vec<_>, (Error, (usize, usize))>>()?;

        if global_bit_pos % 8 != 0 {
            // header error 4.1: invalid header length, not dividable by 8
            let reason = format!("invalid header bit length {}", global_bit_pos);
            return_err_1!((Error::header(reason), header_pos))
        } else if global_bit_pos / 8 > MAX_MTU_IN_BYTES {
            // header error 4.2: invalid header length, too large, exceed max MTU size
            let reason = format!(
                "header byte length {} is exceeds the maximum MTU size {}",
                global_bit_pos / 8,
                MAX_MTU_IN_BYTES
            );
            return_err_1!((Error::header(reason), header_pos))
        } else {
            Ok(Self {
                header_len_in_bytes: (global_bit_pos / 8) as usize,
                field_list,
                field_position,
            })
        }
    }

    pub fn field_iter(&self) -> impl Iterator<Item = (&Field, BitPos)> {
        self.field_list
            .iter()
            .map(|(name, field)| (field, self.field_position.get(name).unwrap().0))
    }

    pub fn field(&self, s: &'_ str) -> Option<(&Field, BitPos)> {
        let (bit_pos, index) = self.field_position.get(s)?;

        Some((&self.field_list[*index].1, *bit_pos))
    }

    pub fn header_len_in_bytes(&self) -> usize {
        self.header_len_in_bytes
    }
}

/// BitPos records the starting and ending position of a header field.
///
/// An example of the header field position:
/// ```
/// byte position:  0               1
/// bit position:   0 1 2 3 4 5 6 7 0 1 2  3  4  5  6  7
/// global bit pos: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
///                 ^                 ^
///           start BitPos       end BitPos
/// ```
/// Note: two BitPos can form a range, indicating the starting position and
/// ending position of the header field. This range is includsive by default.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct BitPos {
    pub byte_pos: u64,
    pub bit_pos: u64,
}

impl BitPos {
    pub(crate) fn new(global_bit_pos: u64) -> Self {
        Self {
            byte_pos: global_bit_pos / 8,
            bit_pos: global_bit_pos % 8,
        }
    }

    pub(crate) fn to_global_pos(&self) -> u64 {
        self.byte_pos * 8 + self.bit_pos
    }

    pub(crate) fn next_pos(&self, len: u64) -> Self {
        Self::new(self.to_global_pos() + len - 1)
    }
}
