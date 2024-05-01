use std::collections::HashMap;
use std::io::{Read, Write};

use crate::utils::Spanned;

const RESERVED_FIELD_NAMES: &[&str] = &["header_len", "payload_len", "packet_len"];

quick_error! {
    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub enum Error {
        InvalidField(reason: &'static str) {
            display("invalid Field definition: {}", reason)
        }
        InvalidHeader(reason: &'static str) {
            display("invalid header list: {}", reason)
        }
    }
}

const ERR_REASON_FIELD1: &str = r#"invalid repr
If repr is defined, it must satisfy the following conditions:
1. The defined repr is the same as the repr inferred from the bit.
2. The defined repr is a byte slice, and bit % 8 == 0"#;

const ERR_REASON_FIELD2: &str = r#"invalid arg
If arg is defined, it must satisfy one of the following conditions:
1. The defined arg is the same as the current repr (either defined or inferred).
2. The defined arg is bool, and the bit size is 1.
3. The defined arg is a standard Rust type wrapped by %% as code segment."#;

const ERR_REASON_FIELD3: &str = r#"missing default
The arg (either defined or inferred) is a Rust code segment, and the default must be 
provided as a Rust code segment."#;

const ERR_REASON_FIELD4: &str = r#"invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number."#;

const ERR_REASON_FIELD5: &str = r#"invalid default
If arg is a code segment, then the default must be a code segment as well."#;

const ERR_REASON_FIELD6: &str = r#"invalid bit size
the following expression should not hold: 
bit == 0 || (bit > 64 && bit % 8 != 0)"#;

const ERR_REASON_HEADER1: &str = r#"duplicated field name
Each field name defined in the header should be unique."#;

const ERR_REASON_HEADER2: &str = r#"not aligned to byte boundary
The total bit size of the header fields should be dividable by 8."#;

const ERR_REASON_HEADER3: &str = r#"not aligned to byte boundary
If the header field contains multiple bytes, then one of the two ends 
must be aligned to the byte boudaries."#;

const ERR_REASON_HEADER4: &str = r#"invalid field name
header_len, payload_len and packet_len are reserved field names"#;

const ERR_REASON_LENGTH1: &str = r#"wtf?"#;

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
            _ => panic!(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Arg {
    BuiltinTypes(BuiltinTypes),
    Code(String),
}

impl Arg {
    pub fn to_string(&self) -> String {
        match self {
            Arg::BuiltinTypes(bt) => bt.to_string(),
            Arg::Code(code_str) => code_str.to_string(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum DefaultVal {
    Num(u64),
    Bool(bool),
    ZeroBytes,
    Code(String),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Field {
    pub bit: u64,
    pub repr: BuiltinTypes,
    pub arg: Arg,
    pub default: DefaultVal,
    pub gen: bool,
}

impl Field {
    // Infer repr from bit if repf is not defined
    pub(crate) fn infer_repr(bit: u64) -> BuiltinTypes {
        // Makesure that bit is positive
        assert!(bit > 0);

        if bit <= 8 {
            BuiltinTypes::U8
        } else if bit <= 16 {
            BuiltinTypes::U16
        } else if bit <= 32 {
            BuiltinTypes::U32
        } else if bit <= 64 {
            BuiltinTypes::U64
        } else {
            // If bit > 64, then bit % 8 == 0
            assert!(bit % 8 == 0);
            BuiltinTypes::ByteSlice
        }
    }

    // Check whether the defined repr complies with the bit
    fn check_defined_repr(bit: u64, defined_repr: &BuiltinTypes) -> Result<BuiltinTypes, Error> {
        let inferred = Self::infer_repr(bit);

        if inferred == *defined_repr {
            // OK: defined repr is the same as the inferred repr
            Ok(*defined_repr)
        } else if *defined_repr == BuiltinTypes::ByteSlice && bit > 8 && bit % 8 == 0 {
            // OK: use &[u8] to override the inferred repr
            Ok(*defined_repr)
        } else {
            return_err!(Error, InvalidField, ERR_REASON_FIELD1)
        }
    }

    // Check whether the defined arg complies with both bit and repr
    fn check_defined_arg(bit: u64, repr: &BuiltinTypes, defined_arg: &Arg) -> Result<Arg, Error> {
        match defined_arg {
            Arg::BuiltinTypes(defined_arg) => {
                if *defined_arg == *repr {
                    // Ok: defined arg is the same as the repr
                    Ok(Arg::BuiltinTypes(*defined_arg))
                } else if *defined_arg == BuiltinTypes::Bool && bit == 1 {
                    // Ok: defined arg is bool while bit size is 1
                    Ok(Arg::BuiltinTypes(*defined_arg))
                } else {
                    return_err!(Error, InvalidField, ERR_REASON_FIELD2)
                }
            }
            // Ok: defined arg is code
            Arg::Code(code_str) => Ok(Arg::Code(code_str.clone())),
        }
    }

    // Infer default from bit and arg
    fn infer_default_val(arg: &Arg) -> Result<DefaultVal, Error> {
        match arg {
            Arg::BuiltinTypes(bt) => {
                match bt {
                    // Ok: Arg is Bool, default to true
                    BuiltinTypes::Bool => Ok(DefaultVal::Bool(bool::default())),

                    // Ok: Arg is ByteSlice, default to ZeroBytes
                    // The length of the bytes can be calculated as bit / 8
                    BuiltinTypes::ByteSlice => Ok(DefaultVal::ZeroBytes),

                    // Ok: Arg is u8/u16/u32/u64, default to 0
                    _ => Ok(DefaultVal::Num(u64::default())),
                }
            }
            Arg::Code(_) => return_err!(Error, InvalidField, ERR_REASON_FIELD3),
        }
    }

    fn check_defined_default_val(
        arg: &Arg,
        defined_default: &DefaultVal,
    ) -> Result<DefaultVal, Error> {
        match arg {
            Arg::BuiltinTypes(bt) => match defined_default {
                // Ok: defined default and arg are both bool
                DefaultVal::Bool(_) if *bt == BuiltinTypes::Bool => Ok(defined_default.clone()),

                // Ok: defined default is code while arg is byte slice
                DefaultVal::Code(_) if *bt == BuiltinTypes::ByteSlice => {
                    Ok(defined_default.clone())
                }

                // Ok: defined default and arg are both num
                DefaultVal::Num(_)
                    if (*bt != BuiltinTypes::Bool && *bt != BuiltinTypes::ByteSlice) =>
                {
                    Ok(defined_default.clone())
                }
                _ => return_err!(Error, InvalidField, ERR_REASON_FIELD4),
            },
            Arg::Code(_) => match defined_default {
                // Ok: defined default and arg are both code
                DefaultVal::Code(_) => Ok(defined_default.clone()),
                _ => return_err!(Error, InvalidField, ERR_REASON_FIELD5),
            },
        }
    }

    pub fn new(
        bit: u64,
        repr: Option<BuiltinTypes>,
        arg: Option<Arg>,
        default: Option<DefaultVal>,
        gen: Option<bool>,
    ) -> Result<Self, Error> {
        if bit == 0 || (bit > 64 && bit % 8 != 0) {
            return_err!(Error, InvalidField, ERR_REASON_FIELD6)
        }

        let repr = match repr {
            Some(defined_repr) => Self::check_defined_repr(bit, &defined_repr)?,
            None => Self::infer_repr(bit),
        };

        let arg = match arg {
            Some(defined_arg) => Self::check_defined_arg(bit, &repr, &defined_arg)?,
            None => Arg::BuiltinTypes(repr),
        };

        let default = match default {
            Some(defined_default) => Self::check_defined_default_val(&arg, &defined_default)?,
            None => Self::infer_default_val(&arg)?,
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
}

// BitPos records the starting and ending position of a header field.
// An example of the header field position:
// byte position:  0               1
// bit position:   0 1 2 3 4 5 6 7 0 1 2  3  4  5  6  7
// global bit pos: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
//                 ^                 ^
//           start BitPos       end BitPos
// Note: two BitPos can form a range, indicating the starting position
// and ending position of the header field.
// This range is includsive by default.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct BitPos {
    pub byte_pos: u64,
    pub bit_pos: u64,
}

impl BitPos {
    // Calculate the BitPos from the global bit pos
    pub(crate) fn new(global_bit_pos: u64) -> Self {
        Self {
            byte_pos: global_bit_pos / 8,
            bit_pos: global_bit_pos % 8,
        }
    }

    // Calculate the corresponding global bit pos
    pub(crate) fn to_global_pos(&self) -> u64 {
        self.byte_pos * 8 + self.bit_pos
    }

    // Calculate the next BitPos that is `len` bits away
    // from the current BitPos
    pub(crate) fn next_pos(&self, len: u64) -> Self {
        Self::new(self.to_global_pos() + len - 1)
    }
}

/// Given a preparsed header list, check its correctness
/// and return a correct header list
///
/// `hl_pos`: the parsed header list
/// `header_pos`: the location of the header list in the original file
pub fn check_header_list(
    hl_pos: Vec<(Spanned<String>, Field)>,
    header_pos: (usize, usize),
) -> Result<(Vec<(String, Field)>, HashMap<String, (BitPos, usize)>), (Error, (usize, usize))> {
    let mut field_pos = HashMap::new();
    let mut global_bit_pos = 0;
    let v = hl_pos
        .into_iter()
        .enumerate()
        .map(|(field_idx, (sp_str, field))| {
            if field_pos.get(&sp_str.item).is_some() {
                // First, we dedup the header field names.
                Err((Error::InvalidHeader(ERR_REASON_HEADER1), sp_str.span))
            } else if RESERVED_FIELD_NAMES
                .iter()
                .find(|reserved| **reserved == &sp_str.item)
                .is_some()
            {
                Err((Error::InvalidHeader(ERR_REASON_HEADER4), sp_str.span))
            } else {
                // Next, we check whether the bit size of the field
                // is correctly aligned.
                let start = BitPos::new(global_bit_pos);
                let end = start.next_pos(field.bit);

                if field.bit > 8 && start.bit_pos != 0 && end.bit_pos != 7 {
                    // If the header field contains multiple bytes, then one
                    // of two ends must be aligned to the byte boudary.
                    // In this branch, neither of the two ends are aligned to the byte boundary,
                    // we report an error.
                    Err((Error::InvalidHeader(ERR_REASON_HEADER3), sp_str.span))
                } else {
                    // move the global_bit_pos past the current header
                    global_bit_pos += field.bit;
                    field_pos.insert(sp_str.item.clone(), (start, field_idx));
                    Ok((sp_str.item, field))
                }
            }
        })
        .collect::<Result<Vec<_>, (Error, (usize, usize))>>()?;

    // Next, we check whether the total bit size of the header
    // aligns to the byte boundary
    if global_bit_pos % 8 != 0 {
        Err((Error::InvalidHeader(ERR_REASON_HEADER2), header_pos))
    } else {
        Ok((v, field_pos))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AlgOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AlgExpr {
    Num(u64),
    Ident(String),
    Binary(Box<AlgExpr>, AlgOp, Box<AlgExpr>),
}

impl AlgExpr {
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

    pub fn try_take_usable_expr(&self) -> Option<UsableAlgExpr> {
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
}

// Check whether a field can be used in a length expression.
pub(crate) fn check_valid_length_expr(field: &Field) -> bool {
    // A field can only be used in a length expression
    // if the repr is not a byte slice and that the arg
    // is the same as the repr.
    if field.repr != BuiltinTypes::ByteSlice {
        match field.arg {
            Arg::BuiltinTypes(arg) => field.repr == arg,
            _ => false,
        }
    } else {
        false
    }
}

pub enum UsableAlgExpr {
    IdentOnly(String),
    /// String + u64
    SimpleAdd(String, u64),
    /// String + u64
    SimpleMult(String, u64),
    /// (String + u64.1) * u64.2
    AddMult(String, u64, u64),
    /// String * u64.1 + u64.2
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

    /// Given an input variable `x`, calculate the final value of this expression.
    ///
    /// Note: we assume that all the calculations done here will not trigger overflow,
    /// because the parser will ensure that the values are bounded by `COMPILER_MAX_NUM`.
    pub fn exec(&self, x: u64) -> u64 {
        match self {
            Self::IdentOnly(_) => x,
            Self::SimpleAdd(_, add) => x + add,
            Self::SimpleMult(_, mult) => x * mult,
            Self::AddMult(_, add, mult) => (x + add) * mult,
            Self::MultAdd(_, mult, add) => x * mult + add,
        }
    }

    /// Given an result value `y`, do reverse calculation and find out the
    /// corresponding input value.
    ///
    /// Return the input value if it's an integer, or return `None` if it's fractional.
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

    /// Dump a guard condition to the `output` that can be used to protect the reverse calculation
    /// expression.
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

    /// Given a result string `y_str`, dump the expression for reverse calculating the
    /// intput value to the `output`.
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

#[derive(Debug, Clone)]
pub struct LengthInfo {
    pub expr: Box<AlgExpr>,
    pub max: Option<u64>,
}

#[derive(Debug, Clone)]
pub struct Packet {
    pub protocol_name: String,
    pub field_list: Vec<(String, Field)>,
    pub field_pos_map: HashMap<String, (BitPos, usize)>,
    pub header_len_option_name: Option<(LengthInfo, String)>,
    pub payload_len: Option<LengthInfo>,
    pub packet_len: Option<LengthInfo>,
}

impl Packet {
    /// We should perform checks to ensure that the length fields
    /// are correctly defined, but for now, we ignore it.
    pub fn new(
        protocol_name: String,
        field_list: Vec<(String, Field)>,
        field_pos_map: HashMap<String, (BitPos, usize)>,
        header_len_with_pos: Option<(LengthInfo, String, (usize, usize))>,
        payload_len_with_pos: Option<(LengthInfo, (usize, usize))>,
        packet_len_with_pos: Option<(LengthInfo, (usize, usize))>,
    ) -> Result<Self, (Error, (usize, usize))> {
        Ok(Self {
            protocol_name,
            field_list,
            field_pos_map,
            header_len_option_name: header_len_with_pos.map(|t| (t.0, t.1)),
            payload_len: payload_len_with_pos.map(|t| t.0),
            packet_len: packet_len_with_pos.map(|t| t.0),
        })
    }
}
