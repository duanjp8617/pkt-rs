#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub struct Bit(u64);

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Repr {
    U8,
    U16,
    U32,
    U64,
    ByteSlice(u64),
}

impl Repr {
    pub fn infer_from_bit(bit: Bit) -> Self {
        if bit.0 <= 8 {
            Self::U8
        } else if bit.0 <= 16 {
            Self::U16
        } else if bit.0 <= 32 {
            Self::U32
        } else if bit.0 <= 64 {
            Self::U64
        } else {
            if bit.0 % 8 == 0 {
                Self::ByteSlice(bit.0 / 8)
            } else {
                Self::ByteSlice(bit.0 / 8 + 1)
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Arg {
    BuiltinType(Repr),
    CustomType(String),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum DefaultVal {
    UInt(u64),
    ByteArray(Vec<u8>),
    TypeVal(String),
}

impl DefaultVal {
    pub fn infer_from_arg(repr: &Repr) -> Self {
        match repr {
            Repr::ByteSlice(sl) => {
                let mut v = Vec::new();
                v.resize(*sl as usize, 0);
                Self::ByteArray(v)
            }
            _ => Self::UInt(0),
        }
    }
}

pub enum OpExpr {
    UInt(u64),
    Ident(String),
    Add(Box<OpExpr>, Box<OpExpr>),
    Sub(Box<OpExpr>, Box<OpExpr>),
    Mult(Box<OpExpr>, Box<OpExpr>),
    Div(Box<OpExpr>, Box<OpExpr>),
}

pub enum CmpExpr {
    Eq(Box<OpExpr>, Box<OpExpr>),
    Ne(Box<OpExpr>, Box<OpExpr>),
    Lt(Box<OpExpr>, Box<OpExpr>),
    Le(Box<OpExpr>, Box<OpExpr>),
    Gt(Box<OpExpr>, Box<OpExpr>),
    Ge(Box<OpExpr>, Box<OpExpr>),
}

pub enum BinopExpr {
    Neg(Box<CmpExpr>),
    And(Box<CmpExpr>, Box<CmpExpr>),
    Or(Box<CmpExpr>, Box<CmpExpr>),
}

pub enum LenType {
    HeaderLen,
    PayloadLen,
    PacketLen,
}

pub struct Field {
    bit: Bit,
    repr: Repr,
    arg: Arg,
    default: DefaultVal,
    gen: bool,
}

pub struct VarField {
    len: OpExpr,
    item_size: Option<u64>,
    arg: Option<String>,
}

pub struct LenField {
    t: LenType,
    expr: OpExpr,
    min: u64,
    max: u64,
}

pub struct Packet {
    header: Vec<Field>,
    var_header: Vec<VarField>,
    with_payload: bool,
    header_len: Option<LenField>,
    payload_len: Option<LenField>,
    packet_len: Option<LenField>,
}

pub struct Message {
    header: Vec<Field>,
    var_header: Vec<VarField>,
    header_len: Option<LenField>,
    cond: Option<BinopExpr>
}