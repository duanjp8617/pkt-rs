use std::collections::HashMap;

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

pub enum BuiltinType {
    U8,
    U16,
    U32,
    U64,
    ByteSlice,
    Bool,
}

pub enum SimpleVal {
    UInt(u64),
    Ident(String),
    BuiltinType(BuiltinType),
    RsType(String),
    RsExpr(String),
    OpExpr(OpExpr),
    BinopExpr(BinopExpr),
}

pub enum Value {
    SimpleVal(SimpleVal),
    List(Vec<(String, Value)>),
    Ctor(Box<Ctor>),
}

pub struct Body(HashMap<String, Value>);

pub struct Ctor {
    name: String,
    body: Box<Body>,
}

pub enum DefType {
    Message,
    Packet,
    IterGroup,
}

pub struct Def {
    t: DefType,
    name: String,
    body: Box<Body>,
}
