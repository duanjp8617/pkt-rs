#[derive(Debug, Clone)]
pub enum AlgExpr {
    Num(u64),
    IdentVar(String),
    Op(Box<AlgExpr>, AlgOp, Box<AlgExpr>),
}

#[derive(Debug, Clone)]
pub enum AlgOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum CmpExpr {
    Bool(bool),
    Op(Box<AlgExpr>, CmpOp, Box<AlgExpr>),
    Neg(Box<CmpExpr>),
    And(Box<CmpExpr>, Box<CmpExpr>),
    Or(Box<CmpExpr>, Box<CmpExpr>),
}

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub enum Primitive {
    IdentType(String),
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
    Primitive(usize, Primitive, usize),
    List(usize, Vec<Box<Assignment>>, usize),
    Ctor(usize, Box<Ctor>, usize),
}

#[derive(Debug)]
pub struct Assignment {
    pub ident_var_with_pos: (usize, String, usize),
    pub value: Box<Value>,
}

#[derive(Debug)]
pub struct Ctor {
    pub ident_type: String,
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
    pub def_type: DefType,
    pub ident_type: String,
    pub list: Vec<Box<Assignment>>,
}