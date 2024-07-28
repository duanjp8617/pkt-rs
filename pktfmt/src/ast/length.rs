use std::convert::TryFrom;
use std::io::Write;

use super::field::{Arg, BuiltinTypes};
use super::header::Header;
use super::max_value;
use super::number::MAX_MTU_IN_BYTES;
use super::Error;

const LENGTH_FIELDS: &[&str; 3] = &["header_len", "payload_len", "packet_len"];

/// The ast type constructed when parsing `length` list from the pktfmt script.
#[derive(Debug)]
pub struct Length {
    length_fields: Vec<LengthField>,
}

impl Length {
    pub fn at(&self, index: usize) -> &LengthField {
        &self.length_fields[index]
    }
}

impl Length {
    /// Create a new `Length` object from the parsed input.
    pub fn from_packet_length(
        length_fields: Vec<LengthField>,
        header: &Header,
    ) -> Result<Self, Error> {
        // `length_fields` is created by the parser, its length is guaranteed to be 3.

        let res = Self { length_fields };
        match (res.at(0), res.at(1), res.at(2)) {
            (LengthField::None, LengthField::None, LengthField::None) => {
                // no length definition, no check is needed
            }
            (_, LengthField::None, LengthField::None) => {
                // the packet has a variable header length
                res.check_length_field(header, 0)?;
            }
            (LengthField::None, _, LengthField::None) => {
                // the packet has a variable payload length
                res.check_length_field(header, 1)?;
            }
            (LengthField::None, LengthField::None, _) => {
                // the packet has a variable packet length
                res.check_length_field(header, 2)?;
            }
            (_, _, LengthField::None) => {
                // the packet has a variable header and payload length
                res.check_length_field(header, 0)?;
                res.check_length_field(header, 1)?;
            }
            (_, LengthField::None, _) => {
                // the packet has a variable header and packet length
                res.check_length_field(header, 0)?;
                res.check_length_field(header, 2)?;
            }
            _ => {
                // length error 1
                return_err!(Error::length(1, "invalid packet length format".to_string()))
            }
        }
        Ok(res)
    }

    // check whether the length field indexed by `index` is correctly defined
    // index: 0 for header_len, 1 for payload_len, 2 for packet_len
    fn check_length_field(&self, header: &Header, index: usize) -> Result<(), Error> {
        let length_field = &self.length_fields[index];
        match Self::check_length_field_basic(header, length_field)? {
            Some((field_bit_size, expr, fixed_length_opt)) => match fixed_length_opt {
                Some(fixed_length) => {
                    if index != 0 {
                        // only header_len can be associated with a fixed length,
                        // length error 2:
                        return_err!(Error::length(
                            2,
                            format!(
                                "invalid fixed length, it can not be associated with {}",
                                LENGTH_FIELDS[index]
                            )
                        ))
                    }

                    if fixed_length > MAX_MTU_IN_BYTES
                        || (fixed_length as usize) < header.header_len_in_bytes()
                    {
                        // length error 3
                        return_err!(Error::length(
                            3,
                            format!(
                                "invalid fixed length {}, it should be in the range [{}, {}]",
                                fixed_length,
                                header.header_len_in_bytes(),
                                MAX_MTU_IN_BYTES
                            )
                        ))
                    }

                    if let None = expr.reverse_exec(fixed_length) {
                        // length error 4
                        return_err!(Error::length(
                            4,
                            format!(
                            "fixed length {} can not be derived from the length field expression",
                            fixed_length
                        )
                        ))
                    }

                    Ok(())
                }
                None => {
                    // the length field has no fixed length, it can be any one
                    // of the header/payload/packet_len, we perform the
                    // following checks

                    let x_max = max_value(field_bit_size).unwrap();
                    // length error 5
                    let max_length = expr.exec(x_max).ok_or(Error::length(
                        5,
                        format!(
                            "the max length can not be calculated for {} using the max field value {}",
                            LENGTH_FIELDS[index],
                            x_max
                        ),
                    ))?;
                    if max_length > MAX_MTU_IN_BYTES {
                        // length error 6
                        return_err!(Error::length(
                            6,
                            format!(
                                "max length {} of {} exceeds MTU limit",
                                max_length, LENGTH_FIELDS[index]
                            )
                        ))
                    }

                    if index == 0 || index == 2 {
                        // if length field denotes header_len or packet_len,
                        // then we make sure that the fixed header length can be
                        // derived from the length expression. if fail, generate
                        // the following error:
                        let header_len = header.header_len_in_bytes() as u64;
                        if let None = expr.reverse_exec(header_len) {
                            // length error 7
                            return_err!(Error::length(
                                7,
                                format!(
                                    "header length {} can not be derived from the {} expression",
                                    header_len, LENGTH_FIELDS[index]
                                )
                            ))
                        }
                    }

                    Ok(())
                }
            },
            None => {
                // the length expression is not defined, we can bypass the check and directly
                // return ok
                Ok(())
            }
        }
    }

    // perform basic checks for the length field, return the field bit size if the
    // field used for length calculation is present.
    fn check_length_field_basic<'parsed_object>(
        header: &'parsed_object Header,
        length_field: &'parsed_object LengthField,
    ) -> Result<Option<(u64, &'parsed_object UsableAlgExpr, Option<u64>)>, Error> {
        match length_field {
            LengthField::Undefined => {
                // length field is present but not defined, there will be no error
                Ok(None)
            }
            LengthField::Expr {
                expr,
                fixed_length_opt,
            } => {
                // make sure that the field name contained in the expr correspond to a field
                // in the header, if it fails, we generate:
                // length error 8
                let name = expr.field_name();
                let (field, _) = header.field(name).ok_or(Error::length(
                    8,
                    format!("invalid length expression field name {}", name),
                ))?;

                // make sure that the bit size of the field used for length calculation does not
                // exceeds the size of usize.
                if field.bit > (std::mem::size_of::<usize>() as u64) * 8 {
                    // length error 11
                    return_err!(Error::length(
                        11,
                        format!(
                            "the bit size {} of length field {} exceeds the bit size {} of usize",
                            field.bit,
                            name,
                            (std::mem::size_of::<usize>()) * 8
                        )
                    ))
                }

                // A field can only be used in a length expression if the repr is not a byte
                // slice and that the arg is the same as the repr.
                match field.arg {
                    Arg::BuiltinTypes(arg)
                        if (field.repr != BuiltinTypes::ByteSlice) && (field.repr == arg) =>
                    {
                        // A field can only be used in a length expression if
                        // the repr is not a byte slice and that the arg is the
                        // same as the repr.
                        Ok(Some((field.bit, expr, *fixed_length_opt)))
                    }
                    _ => {
                        // length error 9
                        Err(Error::length(
                            9,
                            format!(
                                "the field used by length expression is invalid: {:?}",
                                field
                            ),
                        ))
                    }
                }
            }
            _ => {
                // here, the field must not be none
                panic!()
            }
        }
    }
}

/// The ast type constructed when parsing length list.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LengthField {
    /// The packet has no such length field
    None,

    /// The packet has the length field, but the calculating expression is not
    /// defined
    Undefined,

    /// The packet has the length field calculated from the `UsableAlgExpr`,
    /// together with an optional fixed length value
    Expr {
        expr: UsableAlgExpr,
        fixed_length_opt: Option<u64>,
    },
}

impl LengthField {
    /// Check whether the length field appears in the packet.
    pub fn appear(&self) -> bool {
        match self {
            Self::None => false,
            _ => true,
        }
    }

    /// Try to acquire a `UsableAlgExpr` from the length field.
    ///
    /// The method returns `None` if the length field does not appear, or the
    /// length field expression is not defined.
    pub fn try_get_expr(&self) -> Option<&UsableAlgExpr> {
        match self {
            Self::Expr {
                expr,
                fixed_length_opt: _,
            } => Some(expr),
            _ => None,
        }
    }

    pub(crate) fn from_option(expr_option: Option<Self>) -> Self {
        match expr_option {
            Some(inner) => inner,
            None => Self::Undefined,
        }
    }
}

/// An enum type that only represents a subset of the general-purpose
/// algorithmic expressions.
///
/// It represents how the packet length is calculated from a header field.
///
/// The code will be a lot more complex if we process general-purpose
/// algorithmic expressions. So we only handle a subset of the expressions.
///
/// The subset of expressions are useful enough to calculate the packet length
/// of many protocols.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UsableAlgExpr {
    /// field_id
    IdentOnly(String),

    /// field_id + u64
    SimpleAdd(String, u64),

    /// field_id * u64
    SimpleMult(String, u64),

    /// (field_id + u64_0) * u64_1
    AddMult(String, u64, u64),

    /// field_id * u64.0 + u64.1
    MultAdd(String, u64, u64),
}

impl UsableAlgExpr {
    /// Get the field name contained in the expression.
    pub fn field_name(&self) -> &str {
        match self {
            Self::IdentOnly(s)
            | Self::SimpleAdd(s, _)
            | Self::SimpleMult(s, _)
            | Self::AddMult(s, _, _)
            | Self::MultAdd(s, _, _) => s,
        }
    }

    /// Substitute input value `x` with `field_id`,  calculate the final value
    /// of this expression.
    ///
    /// Return `None` if overload happens.
    pub fn exec(&self, x: u64) -> Option<u64> {
        match self {
            Self::IdentOnly(_) => Some(x),
            Self::SimpleAdd(_, add) => x.checked_add(*add),
            Self::SimpleMult(_, mult) => x.checked_mul(*mult),
            Self::AddMult(_, add, mult) => x.checked_add(*add)?.checked_mul(*mult),
            Self::MultAdd(_, mult, add) => x.checked_mul(*mult)?.checked_add(*add),
        }
    }

    /// Given an result value `y`, do reverse calculation and find out the
    /// corresponding input value.
    ///
    /// Return the input value if it's an integer, or return `None` if we can't
    /// derive an integer input value.
    pub fn reverse_exec(&self, y: u64) -> Option<u64> {
        match self {
            Self::IdentOnly(_) => Some(y),
            Self::SimpleAdd(_, add) => {
                if y >= *add {
                    Some(y - add)
                } else {
                    None
                }
            }
            Self::SimpleMult(_, mult) => {
                if y % mult == 0 {
                    Some(y / mult)
                } else {
                    None
                }
            }
            Self::AddMult(_, add, mult) => {
                if y % mult == 0 && (y / mult) >= *add {
                    Some(y / mult - add)
                } else {
                    None
                }
            }
            Self::MultAdd(_, mult, add) => {
                if y >= *add && (y - add) % mult == 0 {
                    Some((y - add) / mult)
                } else {
                    None
                }
            }
        }
    }

    /// Given an input string `x_str`, dump the expression for calculating the
    /// result value to the `output`.
    pub fn gen_exec(&self, x_str: &str, output: &mut dyn Write) {
        let res = match self {
            Self::IdentOnly(_) => write!(output, "{}", x_str),
            Self::SimpleAdd(_, add) => write!(output, "{}+{}", x_str, add),
            Self::SimpleMult(_, mult) => write!(output, "{}*{}", x_str, mult),
            Self::AddMult(_, add, mult) => write!(output, "({}+{})*{}", x_str, add, mult),
            Self::MultAdd(_, mult, add) => write!(output, "{}*{}+{}", x_str, mult, add),
        };
        res.unwrap();
    }

    /// Dump a guard condition to the `output` that can be used to protect the
    /// reverse calculation expression.
    pub fn reverse_exec_guard(&self, y_str: &str) -> String {
        match self {
            Self::SimpleMult(_, mult) | Self::AddMult(_, _, mult) => {
                format!("{}%{}==0", y_str, mult)
            }
            Self::MultAdd(_, mult, add) => {
                format!("({}-{})%{}==0", y_str, add, mult)
            }
            _ => "".to_string(),
        }
    }

    /// Given a result string `y_str`, dump the expression for reverse
    /// calculating the intput value to the `output`.
    pub fn gen_reverse_exec(&self, y_str: &str, output: &mut dyn Write) {
        let res = match self {
            Self::IdentOnly(_) => write!(output, "{}", y_str),
            Self::SimpleAdd(_, add) => write!(output, "{}-{}", y_str, add),
            Self::SimpleMult(_, mult) => write!(output, "{}/{}", y_str, mult),
            Self::AddMult(_, add, mult) => write!(output, "{}/{}-{}", y_str, mult, add),
            Self::MultAdd(_, mult, add) => write!(output, "({}-{})/{}", y_str, add, mult),
        };
        res.unwrap();
    }
}

// the algorithmic operations used in the parser
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum AlgOp {
    Add,
    Sub,
    Mul,
    Div,
}

// the general-purpose algorithmic expression used by the parser
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum AlgExpr {
    Num(u64),
    Ident(String),
    Binary(Box<AlgExpr>, AlgOp, Box<AlgExpr>),
}

impl AlgExpr {
    // length error 10: algorithmic expression is too complex
    pub(crate) fn try_take_usable_alg_expr(&self) -> Result<UsableAlgExpr, Error> {
        self.try_take_all_types().ok_or(Error::field(
            10,
            format!(
            "the form of the algorithmic expression is too complex, only simple ones are supported"
        ),
        ))
    }

    fn try_take_all_types(&self) -> Option<UsableAlgExpr> {
        match self {
            AlgExpr::Binary(left, op, right) => match (&(**left), op, &(**right)) {
                (AlgExpr::Binary(_, _, _), AlgOp::Add, AlgExpr::Num(other)) => {
                    match left.try_take_simple_type()? {
                        UsableAlgExpr::SimpleMult(s, num) => {
                            Some(UsableAlgExpr::MultAdd(s, num, *other))
                        }
                        _ => None,
                    }
                }
                (AlgExpr::Num(other), AlgOp::Add, AlgExpr::Binary(_, _, _)) => {
                    match right.try_take_simple_type()? {
                        UsableAlgExpr::SimpleMult(s, num) => {
                            Some(UsableAlgExpr::MultAdd(s, num, *other))
                        }
                        _ => None,
                    }
                }
                (AlgExpr::Binary(_, _, _), AlgOp::Mul, AlgExpr::Num(other)) => {
                    match left.try_take_simple_type()? {
                        UsableAlgExpr::SimpleAdd(s, num) => {
                            Some(UsableAlgExpr::AddMult(s, num, *other))
                        }
                        _ => None,
                    }
                }
                (AlgExpr::Num(other), AlgOp::Mul, AlgExpr::Binary(_, _, _)) => {
                    match right.try_take_simple_type()? {
                        UsableAlgExpr::SimpleAdd(s, num) => {
                            Some(UsableAlgExpr::AddMult(s, num, *other))
                        }
                        _ => None,
                    }
                }
                _ => self.try_take_simple_type(),
            },
            _ => self.try_take_simple_type(),
        }
    }

    fn try_take_simple_type(&self) -> Option<UsableAlgExpr> {
        match self {
            AlgExpr::Ident(s) => Some(UsableAlgExpr::IdentOnly(s.clone())),
            AlgExpr::Binary(left, op, right) => match (&(**left), op, &(**right)) {
                (AlgExpr::Num(num), AlgOp::Add, AlgExpr::Ident(s)) => {
                    Some(UsableAlgExpr::SimpleAdd(s.clone(), *num))
                }
                (AlgExpr::Ident(s), AlgOp::Add, AlgExpr::Num(num)) => {
                    Some(UsableAlgExpr::SimpleAdd(s.clone(), *num))
                }
                (AlgExpr::Num(num), AlgOp::Mul, AlgExpr::Ident(s)) => {
                    Some(UsableAlgExpr::SimpleMult(s.clone(), *num))
                }
                (AlgExpr::Ident(s), AlgOp::Mul, AlgExpr::Num(num)) => {
                    Some(UsableAlgExpr::SimpleMult(s.clone(), *num))
                }
                _ => None,
            },
            _ => None,
        }
    }
}
