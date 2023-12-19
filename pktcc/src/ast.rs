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

#[derive(Debug)]
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
    IdentT(String),
    BuiltinType(BuiltinType),
    RsExpr(String),
    AlgExpr(Box<AlgExpr>),
    CmpExpr(Box<CmpExpr>),
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
