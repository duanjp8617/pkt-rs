use std::collections::HashMap;

use crate::utils::Spanned;

use super::field::Field;
use super::number::MAX_MTU_IN_BYTES;
use super::Error;

const RESERVED_FIELD_NAMES: &[&str] = &["header_len", "payload_len", "packet_len"];

/// The ast type constructed when parsing `header` list from the pktfmt script.
///
/// **Member fields:**
///
/// `header_len_in_byets`: the length of the fixe header in bytes
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
    /// Create a new `Header` object from the parsed input.
    ///
    /// **Input args:**
    ///
    /// `field_list`: the parsed `header` list
    ///
    /// `header_pos`: the byte indexes of the `header`` list in the original
    /// file
    ///
    /// **Return value:**
    ///
    /// if succeed: a new `Header` object,
    ///
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
                    // header error 1
                    return_err!((
                        Error::header(1, format!("duplicated header field name {}", &sp_str.item)),
                        sp_str.span
                    ))
                } else if RESERVED_FIELD_NAMES
                    .iter()
                    .find(|reserved| **reserved == &sp_str.item)
                    .is_some()
                {
                    // header error 2
                    return_err!((
                        Error::header(
                            2,
                            format!(
                                "header field name {} is reserved and can't be used",
                                &sp_str.item
                            )
                        ),
                        sp_str.span
                    ))
                } else {
                    // calculate the start and end bit position of the header
                    let start = BitPos::new(global_bit_pos);
                    let end = start.next_pos(field.bit);

                    if field.bit > 8 && start.bit_pos != 0 && end.bit_pos != 7 {
                        // header error 3
                        // If the header field contains multiple bytes, then one of two ends must be
                        // aligned to the byte boudary. In this branch, neither of
                        // the two ends are aligned to the byte boundary, we report an error.
                        return_err!((
                            Error::header(
                                3,
                                format!(
                            "header field {} is not correctly aligned to the byte boundaries",
                            &sp_str.item
                        )
                            ),
                            sp_str.span
                        ))
                    } else {
                        global_bit_pos += field.bit;
                        field_position.insert(sp_str.item.clone(), (start, field_idx));
                        Ok((sp_str.item, field))
                    }
                }
            })
            .collect::<Result<Vec<_>, (Error, (usize, usize))>>()?;

        if global_bit_pos % 8 != 0 {
            // header error 4
            return_err!((
                Error::header(
                    4,
                    format!(
                        "invalid header bit length {}, not dividable by 8",
                        global_bit_pos
                    )
                ),
                header_pos
            ))
        } else if global_bit_pos / 8 > MAX_MTU_IN_BYTES {
            // header error 5
            return_err!((
                Error::header(
                    5,
                    format!(
                        "header byte length {} is exceeds the maximum MTU size {}",
                        global_bit_pos / 8,
                        MAX_MTU_IN_BYTES
                    )
                ),
                header_pos
            ))
        } else {
            Ok(Self {
                header_len_in_bytes: (global_bit_pos / 8) as usize,
                field_list,
                field_position,
            })
        }
    }

    /// Return an iterator that generates each `Field` and start `BitPos` of
    /// each `Field`.
    pub fn field_iter(&self) -> impl Iterator<Item = (&Field, BitPos)> {
        self.field_list
            .iter()
            .map(|(name, field)| (field, self.field_position.get(name).unwrap().0))
    }

    /// Given a field name `s`, return the corresponding `Field`.
    pub fn field(&self, s: &'_ str) -> Option<(&Field, BitPos)> {
        let (bit_pos, index) = self.field_position.get(s)?;

        Some((&self.field_list[*index].1, *bit_pos))
    }

    /// Get the length of the fixed header in bytes.
    pub fn header_len_in_bytes(&self) -> usize {
        self.header_len_in_bytes
    }
}

/// BitPos records the starting and ending position of a header field.
///
/// An example of the header field position:
/// ```text
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
