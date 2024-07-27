use std::fmt;

use super::{max_value, Error};

/// The ast type constructed when parsing `Field` definition.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub bit: u64,
    pub repr: BuiltinTypes,
    pub arg: Arg,
    pub default: DefaultVal,
    pub gen: bool,
}

impl Field {
    /// Create a new `Field` object from the parsed input.
    ///
    /// Except for the `bit`, all the subfields can be optional.
    ///
    /// 1. If a subfield is defined, we check whether it with the inferred
    ///    subfield value.
    ///
    /// 2. if a subfield is not defined, we infer it with the available parsing
    ///    informatin.
    pub fn new(
        bit: u64,
        repr: Option<BuiltinTypes>,
        arg: Option<Arg>,
        default: Option<DefaultVal>,
        gen: Option<bool>,
    ) -> Result<Self, Error> {
        if bit == 0 || (bit > 64 && bit % 8 != 0) {
            // 1. bit size 0 is not allowed.
            // 2. if bit size > 64, then bit size must be aligned to 8.

            // field error 1
            return_err!(Error::field(
                1,
                format!(
"invalid bit {}, bit value must satisfy: bit > 0 && (bit <= 64 | bit % 8 == 0) ", bit)
            ))
        }

        let repr = match repr {
            Some(defined_repr) => Self::check_defined_repr(bit, defined_repr)?,
            None => Self::infer_repr(bit),
        };

        let arg = match arg {
            Some(defined_arg) => Self::check_defined_arg(bit, &repr, defined_arg)?,
            None => Arg::BuiltinTypes(repr),
        };

        let default = match default {
            Some(defined_default) => {
                Self::check_defined_default_val(bit, &repr, &arg, defined_default)?
            }
            None => Self::infer_default_val(bit, &repr, &arg),
        };

        let gen = gen.unwrap_or(true);

        Ok(Field {
            bit,
            repr,
            arg,
            default,
            gen,
        })
    }

    // Infer repr from bit if repf is not defined
    fn infer_repr(bit: u64) -> BuiltinTypes {
        if bit <= 8 {
            BuiltinTypes::U8
        } else if bit <= 16 {
            BuiltinTypes::U16
        } else if bit <= 32 {
            BuiltinTypes::U32
        } else if bit <= 64 {
            BuiltinTypes::U64
        } else {
            BuiltinTypes::ByteSlice
        }
    }

    // check whether the defined repr complies with the inferred repr and the bit
    // size
    fn check_defined_repr(bit: u64, defined_repr: BuiltinTypes) -> Result<BuiltinTypes, Error> {
        let inferred = Self::infer_repr(bit);

        if inferred == defined_repr {
            // OK: defined repr is the same as the inferred repr
            Ok(defined_repr)
        } else if defined_repr == BuiltinTypes::ByteSlice && bit > 8 && bit % 8 == 0 {
            // OK: use &[u8] to override the inferred repr
            Ok(defined_repr)
        } else {
            // field error 2
            return_err!(Error::field(
                2,
                format!(
                    "invalid repr {}, repr should be {}",
                    defined_repr.to_string(),
                    inferred.to_string()
                )
            ))
        }
    }

    // Check whether the defined arg complies with both bit and repr
    fn check_defined_arg(bit: u64, repr: &BuiltinTypes, defined_arg: Arg) -> Result<Arg, Error> {
        match &defined_arg {
            Arg::BuiltinTypes(bt_ref) => {
                if *bt_ref == *repr {
                    // Ok: defined arg is the same as the repr
                    Ok(defined_arg)
                } else if *bt_ref == BuiltinTypes::Bool && bit == 1 {
                    // Ok: defined arg is bool while bit size is 1
                    Ok(defined_arg)
                } else {
                    // field error 3
                    return_err!(Error::field(
                        3,
                        format!(
                            "invalid arg {} under repr {}",
                            defined_arg.to_string(),
                            repr.to_string()
                        )
                    ))
                }
            }
            // Ok: defined arg is code
            Arg::Code(_) => Ok(defined_arg),
        }
    }

    // Infer default value from bit, repr and arg
    fn infer_default_val(bit: u64, repr: &BuiltinTypes, arg: &Arg) -> DefaultVal {
        match repr {
            // Arg is is over-written with Bool, default to true
            BuiltinTypes::U8 if arg == &Arg::BuiltinTypes(BuiltinTypes::Bool) => {
                DefaultVal::Bool(bool::default())
            }
            // Arg is ByteSlice, default to Bytes.
            // The length of the bytes can be calculated as bit / 8
            BuiltinTypes::ByteSlice => {
                let mut bytes = Vec::new();
                bytes.resize((bit / 8) as usize, u8::default());
                DefaultVal::Bytes(bytes)
            }
            // Arg is u8/u16/u32/u64, default to 0
            _ => DefaultVal::Num(u64::default()),
        }
    }

    // check whether the defined default value complies with bit, repr and arg
    fn check_defined_default_val(
        bit: u64,
        repr: &BuiltinTypes,
        arg: &Arg,
        defined_default: DefaultVal,
    ) -> Result<DefaultVal, Error> {
        match repr {
            BuiltinTypes::U8 if arg == &Arg::BuiltinTypes(BuiltinTypes::Bool) => {
                match &defined_default {
                    // Ok: Arg is is over-written with Bool, default could be bool
                    DefaultVal::Bool(_) => Ok(defined_default),
                    _ => {
                        // field error 4
                        return_err!(Error::field(
                            4,
                            format!(
                                "invalid default {} for boolean arg, should be true or false",
                                defined_default
                            )
                        ))
                    }
                }
            }
            BuiltinTypes::ByteSlice => match &defined_default {
                // Ok: repr is ByteSlice, default to Bytes.
                // The length of the bytes must be bit / 8.
                DefaultVal::Bytes(v) if v.len() == (bit / 8) as usize => Ok(defined_default),
                _ => {
                    // field error 5
                    return_err!(Error::field(
                        5,
                        format!(
                            "invalid default {}, should be {}-byte array",
                            defined_default,
                            bit / 8
                        )
                    ))
                }
            },
            _ => match &defined_default {
                // Ok: repr is u8/u16/u32/u64, default to number.
                // The default number must be smaller than 2^bit.
                DefaultVal::Num(n) if *n <= max_value(bit).unwrap() => Ok(defined_default),
                _ => {
                    // field error 6
                    return_err!(Error::field(
                        6,
                        format!(
                            "invalid default {} , should be no larger than {}",
                            defined_default,
                            max_value(bit).unwrap()
                        )
                    ))
                }
            },
        }
    }
}

/// The built-in types of pktfmt script, representing values of `repr` and
/// `arg`.
///
/// The meaning of each arm and how it is translated to rust type are as
/// follows:
///
/// `Self::U8/16/32/64` -> `u8/16/32/64` primitive types.
///
/// `Self::ByteSlice` -> `&[u8]` byte slice type.
///
/// `[Self::Bool]` -> `bool` type.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BuiltinTypes {
    U8,
    U16,
    U32,
    U64,
    ByteSlice,
    Bool,
}

impl BuiltinTypes {
    /// Convert `BuiltinTypes` to `String`.
    pub fn to_string(&self) -> String {
        match self {
            BuiltinTypes::U8 => "u8".to_string(),
            BuiltinTypes::U16 => "u16".to_string(),
            BuiltinTypes::U32 => "u32".to_string(),
            BuiltinTypes::U64 => "u64".to_string(),
            BuiltinTypes::ByteSlice => "&[u8]".to_string(),
            BuiltinTypes::Bool => "bool".to_string(),
        }
    }
}

impl From<&str> for BuiltinTypes {
    fn from(value: &str) -> Self {
        match value {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "&[u8]" => Self::ByteSlice,
            "bool" => Self::Bool,
            _ => panic!("can not convert to BuiltinTypes"),
        }
    }
}

/// The ast type for `arg`, it can be either `BuiltinTypes`, or a `String`.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Arg {
    BuiltinTypes(BuiltinTypes),
    Code(String),
}

impl Arg {
    /// Convert `Arg` to `String`.
    pub fn to_string(&self) -> String {
        match self {
            Arg::BuiltinTypes(bt) => bt.to_string(),
            Arg::Code(code_str) => code_str.to_string(),
        }
    }
}

/// The ast type for `default`.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DefaultVal {
    /// the default value if  `repr` type is `U8/16/32/64`
    Num(u64),
    /// the default value if `repr` type is `u8` and `arg` type is `Bool`
    Bool(bool),
    /// the default value if `repr` type is `ByteSlice`
    Bytes(Vec<u8>),
}

impl fmt::Display for DefaultVal {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(n) => write!(fmt, "{}", n),
            Self::Bool(b) => write!(fmt, "{}", b),
            Self::Bytes(v) => write!(fmt, "{:?}", &v[..]),
        }
    }
}
