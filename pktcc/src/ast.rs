#[derive(Debug)]
pub enum AlgExpr {
    Num(u64),
    IdentV(String),
    Op(Box<AlgExpr>, AlgOp, Box<AlgExpr>),
}

#[derive(Debug)]
pub enum AlgOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum CmpExpr {
    Bool(bool),
    Op(Box<AlgExpr>, CmpOp, Box<AlgExpr>),
    Neg(Box<CmpExpr>),
    And(Box<CmpExpr>, Box<CmpExpr>),
    Or(Box<CmpExpr>, Box<CmpExpr>),
}

#[derive(Debug)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BuiltinType {
    U8,
    U16,
    U32,
    U64,
    ByteSlice,
    Bool,
}

impl BuiltinType {
    // given a valid bit size, convert it to a BuiltinType
    pub fn infer_from_bit_size(bit_size: u64) -> Self {
        if bit_size <= 8 {
            Self::U8
        } else if bit_size <= 16 {
            Self::U16
        } else if bit_size <= 32 {
            Self::U32
        } else if bit_size <= 64 {
            Self::U64
        } else {
            Self::ByteSlice
        }
    }
}

#[derive(Debug)]
pub enum Primitive {
    IdentT(String),
    BuiltinType(BuiltinType),
    RsExpr(String),
    AlgExpr(Box<AlgExpr>),
    CmpExpr(Box<CmpExpr>),
}

impl Primitive {
    pub fn try_take_num(&self) -> Option<u64> {
        match self {
            Self::AlgExpr(ae) => match &**ae {
                AlgExpr::Num(n) => Some(*n),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn try_take_bool_val(&self) -> Option<bool> {
        match self {
            Self::CmpExpr(ce) => match &**ce {
                CmpExpr::Bool(b) => Some(*b),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Primitive(Primitive),
    List(Vec<Box<Assignment>>),
    Ctor(Box<Ctor>),
}

#[derive(Debug)]
pub struct Assignment {
    pub identv: String,
    pub value: Box<Value>,
}

#[derive(Debug)]
pub struct Ctor {
    pub identt: String,
    pub list: Vec<Box<Assignment>>,
}

#[derive(Debug)]
pub enum DefType {
    Message,
    Packet,
    IterGroup,
}

#[derive(Debug)]
pub struct Definition {
    pub deft: DefType,
    pub identt: String,
    pub list: Vec<Box<Assignment>>,
}
