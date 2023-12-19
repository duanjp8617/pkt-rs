
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
    True,
    False,
}

#[derive(Debug)]
pub enum Primitive {
    IdentT(String),
    BuiltinType(BuiltinType),
    RsExpr(String),
    AlgExpr(Box<AlgExpr>),
    CmpExpr(Box<CmpExpr>),
}

pub enum Value {
    Primitive(Primitive),
    List(Vec<Box<Assignment>>),
    Ctor(Box<Ctor>),
}

pub struct Assignment {
    idv: String,
    value: Box<Value>,
}

pub struct Ctor {
    idt: String,
    list: Vec<Box<Assignment>>,
}

pub enum DefType {
    Message,
    Packet,
    IterGroup,
}

pub struct Definition {
    t: DefType,
    idt: String,
    list: Vec<Box<Assignment>>,
}
