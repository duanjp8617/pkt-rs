use std::collections::{HashMap, HashSet};

use crate::ast;
use crate::error::{ErrorKind, ParseError};

// parse a given value into a target type
trait TargetParser {
    type Target;

    fn parse_target(v: &ast::Value) -> Result<Self::Target, ParseError>;
}

// parse an assignment list into a Vec
trait TargetListParser {
    type TP: TargetParser;

    fn parse_target_list(
        v: &ast::Value,
    ) -> Result<Vec<(String, <Self::TP as TargetParser>::Target)>, ParseError> {
        let l = match v {
            ast::Value::List(l) => l,
            _ => Err(ParseError::new(ErrorKind::Wtf, "not a list"))?,
        };

        let mut dedup = HashSet::new();
        let v = l
            .iter()
            .map(|a| &(**a))
            .map(|a| {
                if !dedup.insert(&a.identv) {
                    Err(ParseError::new(
                        ErrorKind::Wtf,
                        "duplicated list identifier",
                    ))
                } else {
                    Self::TP::parse_target(&a.value).map(|t| (a.identv.clone(), t))
                }
            })
            .collect::<Result<Vec<_>, ParseError>>()?;

        Ok(v)
    }
}

// Convert a list of assignments (body of Constructor/Definition) into a HashMap
trait BodyConvert {
    // Check whether the given identifier on the lhs of an assignment is valid.
    fn check_identifiers(s: &str) -> bool;

    // Perform an initial analysis for a Ctor ast, convert the
    // Ctor ast into a HashMap, where key denotes the identifiers
    // and values are the corresponding Primitive values
    // The returned HashMap will be used for subsequent parsing analysis
    fn convert<'ast, I: Iterator<Item = &'ast ast::Assignment>>(
        v: I,
    ) -> Result<HashMap<&'ast str, &'ast ast::Value>, ParseError> {
        let mut m = HashMap::new();
        for (identv, v) in v.map(|assign| (assign.identv.as_str(), &*assign.value)) {
            // make sure the assigned identifier belong to 1 of the 5:
            // bit, repr, arg, default, gen
            if !Self::check_identifiers(identv) {
                return Err(ParseError::new(
                    ErrorKind::CtorAssignIdent,
                    &format!("invalid identifier: {}", identv),
                ));
            }

            // prevent duplicated identifiers
            m.insert(identv, v).map_or(Ok(()), |_| {
                Err(ParseError::new(
                    ErrorKind::CtorAssignIdent,
                    &format!("duplicated identifier {}", identv),
                ))
            })?;
        }
        Ok(m)
    }
}

fn infer_repr_from_bit(bit: u64) -> ast::BuiltinType {
    if bit <= 8 {
        ast::BuiltinType::U8
    } else if bit <= 16 {
        ast::BuiltinType::U16
    } else if bit <= 32 {
        ast::BuiltinType::U32
    } else if bit <= 64 {
        ast::BuiltinType::U64
    } else {
        ast::BuiltinType::ByteSlice
    }
}

fn define_repr_from_prim(bit: u64, pv: &ast::Primitive) -> Result<ast::BuiltinType, ParseError> {
    match pv {
        ast::Primitive::BuiltinType(defined) => {
            let inferred = infer_repr_from_bit(bit);

            if inferred == *defined {
                // OK: defined repr is the same as the inferred repr
                Ok(*defined)
            } else if *defined == ast::BuiltinType::ByteSlice && bit % 8 == 0 {
                // OK: use &[u8] to override the inferred repr
                Ok(*defined)
            } else {
                // Err: defined repr does not match inferred repr
                Err(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    &format!("{:?} incorrect, should be {:?}", defined, inferred),
                ))
            }
        }
        _ => {
            // Err: value of repr must be a BuiltinType
            Err(ParseError::new(
                ErrorKind::CtorAssignValue,
                "repr should be a BuiltinType",
            ))
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Arg {
    BuiltinType(ast::BuiltinType),
    RsType(String),
}

impl Arg {
    fn define_arg_from_prim(
        bit: u64,
        repr: ast::BuiltinType,
        pv: &ast::Primitive,
    ) -> Result<Self, ParseError> {
        match pv {
            ast::Primitive::BuiltinType(defined) => {
                if *defined == repr {
                    // Ok: defined arg is the same as the repr
                    Ok(Self::BuiltinType(*defined))
                } else if *defined == ast::BuiltinType::Bool && bit == 1 {
                    // Ok: defined arg is bool while bit size is 1
                    Ok(Self::BuiltinType(*defined))
                } else {
                    // Err: defined arg does not match repr
                    Err(ParseError::new(
                        ErrorKind::CtorAssignValue,
                        &format!("{:?} incorrect, should be {:?}", defined, repr),
                    ))
                }
            }
            ast::Primitive::IdentT(rs_type) => {
                // Ok: if arg is a type identifier, then we treat it as a regular rust type
                Ok(Self::RsType(rs_type.clone()))
            }
            _ => {
                // Err: value of arg is incorrect
                Err(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    "arg should be BuiltinType or IdentT",
                ))
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum DefaultVal {
    Num(u64),
    Bool(bool),
    ByteArray(Vec<u8>),
    RsExpr(String),
}

impl DefaultVal {
    fn infer_default_val(bit: u64, arg: &Arg) -> Result<Self, ParseError> {
        // if arg is not defined, it is the same as the repr
        match arg {
            Arg::BuiltinType(bt) => {
                if *bt == ast::BuiltinType::Bool {
                    Ok(DefaultVal::Bool(bool::default()))
                } else if *bt == ast::BuiltinType::ByteSlice {
                    let mut v = Vec::new();
                    (0..(bit / 8) as usize).for_each(|_| v.push(u8::default()));
                    Ok(DefaultVal::ByteArray(v))
                } else {
                    Ok(DefaultVal::Num(u64::default()))
                }
            }
            Arg::RsType(_) => {
                return Err(ParseError::new(
                    ErrorKind::CtorAssignIdent,
                    "Arg is RsType, default must be defined",
                ))
            }
        }
    }

    fn define_default_val_from_prim(arg: &Arg, pv: &ast::Primitive) -> Result<Self, ParseError> {
        match &arg {
            Arg::BuiltinType(bt) => {
                if *bt == ast::BuiltinType::Bool {
                    let b = pv.try_take_bool_val().ok_or(ParseError::new(
                        ErrorKind::CtorAssignValue,
                        "default should be a boolean value",
                    ))?;
                    Ok(DefaultVal::Bool(b))
                } else if *bt == ast::BuiltinType::ByteSlice {
                    match pv {
                        ast::Primitive::RsExpr(s) => Ok(DefaultVal::RsExpr(s.clone())),
                        _ => {
                            return Err(ParseError::new(
                                ErrorKind::CtorAssignValue,
                                "default should be RsExpr",
                            ));
                        }
                    }
                } else {
                    let n = pv.try_take_num().ok_or(ParseError::new(
                        ErrorKind::CtorAssignValue,
                        "default should be a number",
                    ))?;
                    Ok(DefaultVal::Num(n))
                }
            }
            Arg::RsType(_) => match pv {
                ast::Primitive::RsExpr(s) => Ok(DefaultVal::RsExpr(s.clone())),
                _ => Err(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    "default should be RsExpr",
                )),
            },
        }
    }
}

pub struct Field {
    bit: u64,
    repr: ast::BuiltinType,
    arg: Arg,
    default: DefaultVal,
    gen: bool,
}

impl BodyConvert for Field {
    fn check_identifiers(s: &str) -> bool {
        match s {
            "bit" => true,
            "repr" => true,
            "arg" => true,
            "default" => true,
            "gen" => true,
            _ => false,
        }
    }
}

impl TargetParser for Field {
    type Target = Self;

    fn parse_target(v: &ast::Value) -> Result<Self::Target, ParseError> {
        let ctor = match v {
            ast::Value::Ctor(ctor) => &**ctor,
            _ => Err(ParseError::new(
                ErrorKind::CtorType,
                "not a constructor type",
            ))?,
        };

        if &ctor.identt != "Field" {
            return Err(ParseError::new(
                ErrorKind::CtorType,
                &format!("{} is not a valid constructor name", ctor.identt),
            ));
        }

        let m = Self::convert(ctor.list.iter().map(|a| &(**a)))?
            .iter()
            .map(|(k, v)| match v {
                ast::Value::Primitive(pv) => Ok((*k, pv)),
                _ => Err(ParseError::new(ErrorKind::Wtf, "not a primitive value")),
            })
            .collect::<Result<HashMap<_, _>, ParseError>>()?;

        // 1. bit must be defined
        // 2. bit size should not be 0,
        // 3. if bit size is larger than 64, it should be dividable by 8
        let bit = m
            .get("bit")
            .ok_or(ParseError::new(
                ErrorKind::CtorAssignIdent,
                "bit must be defined",
            ))
            .and_then(|pv| {
                pv.try_take_num()
                    .ok_or(ParseError::new(
                        ErrorKind::CtorAssignValue,
                        "bit should be a number",
                    ))
                    .and_then(|n| {
                        if n == 0 {
                            Err(ParseError::new(
                                ErrorKind::CtorAssignValue,
                                "bit cannot be 0",
                            ))
                        } else if n > 64 && n % 8 != 0 {
                            Err(ParseError::new(
                                ErrorKind::CtorAssignValue,
                                &format!("bit ({}) > 64, it should be divisible by 8", n),
                            ))
                        } else {
                            Ok(n)
                        }
                    })
            })?;

        let repr = match m.get("repr") {
            Some(v) => define_repr_from_prim(bit, v)?,
            None => infer_repr_from_bit(bit),
        };

        let arg = match m.get("arg") {
            Some(v) => Arg::define_arg_from_prim(bit, repr, v)?,
            None => Arg::BuiltinType(repr), // if arg is not defined, it is the same as the repr
        };

        let default = match m.get("default") {
            Some(v) => DefaultVal::define_default_val_from_prim(&arg, v)?,
            None => DefaultVal::infer_default_val(bit, &arg)?,
        };

        let gen = match m.get("gen") {
            Some(v) => v.try_take_bool_val().ok_or(ParseError::new(
                ErrorKind::CtorAssignValue,
                "gen should be a boolean value",
            ))?,
            None => true,
        };

        return Ok(Field {
            bit,
            repr,
            arg,
            default,
            gen,
        });
    }
}

impl TargetListParser for Field {
    type TP = Self;
}

pub struct VarField {
    len: Option<Box<ast::AlgExpr>>,
    item_bytes: Option<u64>,
    arg: Option<String>,
}

impl BodyConvert for VarField {
    fn check_identifiers(s: &str) -> bool {
        match s {
            "len" => true,
            "item_bytes" => true,
            "arg" => true,
            _ => false,
        }
    }
}

impl TargetParser for VarField {
    type Target = Self;

    fn parse_target(v: &ast::Value) -> Result<Self::Target, ParseError> {
        let ctor = match v {
            ast::Value::Ctor(ctor) => &**ctor,
            _ => Err(ParseError::new(
                ErrorKind::CtorType,
                "not a constructor type",
            ))?,
        };

        if &ctor.identt != "VarField" {
            return Err(ParseError::new(
                ErrorKind::CtorType,
                &format!("{} is not a valid constructor name", ctor.identt),
            ));
        }

        let m = Self::convert(ctor.list.iter().map(|a| &(**a)))?
            .iter()
            .map(|(k, v)| match v {
                ast::Value::Primitive(pv) => Ok((*k, pv)),
                _ => Err(ParseError::new(ErrorKind::Wtf, "not a primitive value")),
            })
            .collect::<Result<HashMap<_, _>, ParseError>>()?;

        let len = match m.get("len") {
            Some(v) => match v {
                ast::Primitive::AlgExpr(expr) => Some(expr.clone()),
                _ => {
                    return Err(ParseError::new(
                        ErrorKind::CtorAssignValue,
                        "len should be AlgExpr",
                    ))
                }
            },
            None => None,
        };

        let item_bytes = match m.get("item_bytes") {
            Some(v) => v
                .try_take_num()
                .ok_or(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    "item_bytes should be number",
                ))
                .and_then(|n| Ok(Some(n)))?,
            None => None,
        };

        let arg = match m.get("arg") {
            Some(v) => match (v, &item_bytes) {
                (_, None) => {
                    return Err(ParseError::new(
                        ErrorKind::CtorAssignIdent,
                        "arg can not be defined because item_bytes is not defined",
                    ))
                }
                (ast::Primitive::IdentT(s), Some(_)) => Some(s.clone()),
                (_, Some(_)) => {
                    return Err(ParseError::new(
                        ErrorKind::CtorAssignValue,
                        "arg must be IdentT",
                    ))
                }
            },
            None => None,
        };

        Ok(VarField {
            len,
            item_bytes,
            arg,
        })
    }
}

impl TargetListParser for VarField {
    type TP = Self;
}

pub enum LenType {
    HeaderLen,
    PayloadLen,
    PacketLen,
}

impl LenType {
    fn try_from_str(s: &str) -> Option<LenType> {
        match s {
            "HeaderLen" => Some(LenType::HeaderLen),
            "PayloadLen" => Some(LenType::PayloadLen),
            "PacketLen" => Some(LenType::PacketLen),
            _ => None,
        }
    }
}

pub struct LenField {
    t: LenType,
    expr: Box<ast::AlgExpr>,
    min: u64,
    max: u64,
}

impl BodyConvert for LenField {
    fn check_identifiers(s: &str) -> bool {
        match s {
            "expr" => true,
            "min" => true,
            "max" => true,
            _ => false,
        }
    }
}

impl TargetParser for LenField {
    type Target = Self;

    fn parse_target(v: &ast::Value) -> Result<Self::Target, ParseError> {
        let ctor = match v {
            ast::Value::Ctor(ctor) => &**ctor,
            _ => Err(ParseError::new(
                ErrorKind::CtorType,
                "not a constructor type",
            ))?,
        };

        if &ctor.identt != "HeaderLen"
            || &ctor.identt != "PayloadLen"
            || &ctor.identt != "PacketLen"
        {
            return Err(ParseError::new(
                ErrorKind::CtorType,
                &format!("{} is not a valid constructor name", ctor.identt),
            ));
        }

        let m = Self::convert(ctor.list.iter().map(|a| &(**a)))?
            .iter()
            .map(|(k, v)| match v {
                ast::Value::Primitive(pv) => Ok((*k, pv)),
                _ => Err(ParseError::new(ErrorKind::Wtf, "not a primitive value")),
            })
            .collect::<Result<HashMap<_, _>, ParseError>>()?;

        let t = LenType::try_from_str(&ctor.identt).unwrap();

        let expr = m
            .get("expr")
            .ok_or(ParseError::new(
                ErrorKind::CtorAssignIdent,
                &format!("expr must be defined for {}", &ctor.identt),
            ))
            .and_then(|pv| match pv {
                ast::Primitive::AlgExpr(expr) => Ok(expr.clone()),
                _ => Err(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    "expr should be AlgExpr",
                )),
            })?;

        let min = m
            .get("min")
            .ok_or(ParseError::new(
                ErrorKind::CtorAssignIdent,
                &format!("min must be defined for {}", &ctor.identt),
            ))
            .and_then(|pv| {
                pv.try_take_num().ok_or(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    "min should a number",
                ))
            })?;

        let max = m
            .get("max")
            .ok_or(ParseError::new(
                ErrorKind::CtorAssignIdent,
                &format!("max must be defined for {}", &ctor.identt),
            ))
            .and_then(|pv| {
                pv.try_take_num().ok_or(ParseError::new(
                    ErrorKind::CtorAssignValue,
                    "max should be a number",
                ))
            })?;

        Ok(LenField { t, expr, min, max })
    }
}

impl TargetListParser for LenField {
    type TP = Self;
}

pub struct Packet {
    tname: String,
    header: Vec<(String, Field)>,
    var_header: Vec<(String, VarField)>,
    with_payload: bool,
    header_len: Option<LenField>,
    payload_len: Option<LenField>,
    packet_len: Option<LenField>,
}

impl BodyConvert for Packet {
    fn check_identifiers(s: &str) -> bool {
        match s {
            "header" => true,
            "var_header" => true,
            "with_payload" => true,
            "header_len" => true,
            "payload_len" => true,
            "packet_len" => true,
            _ => false,
        }
    }
}

impl Packet {
    pub fn from_definition(def: &ast::Definition) -> Result<Self, ParseError> {
        match def.deft {
            ast::DefType::Packet => {}
            _ => Err(ParseError::new(ErrorKind::Wtf, "not a packet definition"))?,
        }

        let m = Self::convert(def.list.iter().map(|a| &(**a)))?;

        let header = m
            .get("header")
            .ok_or(ParseError::new(ErrorKind::Wtf, "header must be defined"))
            .and_then(|v| Field::parse_target_list(*v))?;

        let var_header = m
            .get("var_header")
            .map(|v| VarField::parse_target_list(*v))
            .unwrap_or(Ok(Vec::new()))?;

        let with_payload = m
            .get("with_payload")
            .map(|v| match v {
                &ast::Value::Primitive(pv) => pv
                    .try_take_bool_val()
                    .ok_or(ParseError::new(ErrorKind::Wtf, "not a bool")),
                _ => Err(ParseError::new(ErrorKind::Wtf, "not a primitive value")),
            })
            .unwrap_or(Ok(true))?;

        let header_len = match m.get("header_len") {
            None => None,
            Some(v) => {
                let lf = LenField::parse_target(*v).and_then(|lf| match lf.t {
                    LenType::HeaderLen => Ok(lf),
                    _ => Err(ParseError::new(ErrorKind::Wtf, "not a HeaderLen")),
                })?;
                Some(lf)
            }
        };

        let payload_len = match m.get("payload_len") {
            None => None,
            Some(v) => {
                let lf = LenField::parse_target(*v).and_then(|lf| match lf.t {
                    LenType::PayloadLen => Ok(lf),
                    _ => Err(ParseError::new(ErrorKind::Wtf, "not a PayloadLen")),
                })?;
                Some(lf)
            }
        };

        let packet_len = match m.get("packet_len") {
            None => None,
            Some(v) => {
                let lf = LenField::parse_target(*v).and_then(|lf| match lf.t {
                    LenType::PacketLen => Ok(lf),
                    _ => Err(ParseError::new(ErrorKind::Wtf, "not a PacketLen")),
                })?;
                Some(lf)
            }
        };

        return Ok(Self {
            tname: def.identt.clone(),
            header,
            var_header,
            with_payload,
            header_len,
            payload_len,
            packet_len,
        });
    }
}

pub struct Message {
    tname: String,
    header: Vec<(String, Field)>,
    var_header: Vec<(String, VarField)>,
    header_len: Option<LenField>,
    cond: Option<Box<ast::CmpExpr>>,
}

impl BodyConvert for Message {
    fn check_identifiers(s: &str) -> bool {
        match s {
            "header" => true,
            "var_header" => true,
            "header_len" => true,
            "cond" => true,
            _ => false,
        }
    }
}

impl Message {
    fn from_definition(def: &ast::Definition) -> Result<Self, ParseError> {
        let m = Self::convert(def.list.iter().map(|a| &(**a)))?;

        let header = m
            .get("header")
            .ok_or(ParseError::new(ErrorKind::Wtf, "header must be defined"))
            .and_then(|v| Field::parse_target_list(*v))?;

        let var_header = m
            .get("var_header")
            .map(|v| VarField::parse_target_list(*v))
            .unwrap_or(Ok(Vec::new()))?;

        let header_len = match m.get("header_len") {
            None => None,
            Some(v) => {
                let lf = LenField::parse_target(*v).and_then(|lf| match lf.t {
                    LenType::HeaderLen => Ok(lf),
                    _ => Err(ParseError::new(ErrorKind::Wtf, "not a HeaderLen")),
                })?;
                Some(lf)
            }
        };

        let cond = match m.get("header") {
            None => None,
            Some(v) => match v {
                &ast::Value::Primitive(pv) => match pv {
                    ast::Primitive::CmpExpr(expr) => Some(expr.clone()),
                    _ => Err(ParseError::new(ErrorKind::Wtf, "not a HeaderLen"))?,
                },
                _ => Err(ParseError::new(ErrorKind::Wtf, "not a HeaderLen"))?,
            },
        };

        Ok(Message {
            tname: def.identt.clone(),
            header,
            var_header,
            header_len,
            cond,
        })
    }
}

pub struct IterGroup {
    tname: String,
    messages: Vec<(String, String)>,
}

impl BodyConvert for IterGroup {
    fn check_identifiers(s: &str) -> bool {
        match s {
            "messages" => true,
            _ => false,
        }
    }
}

struct TnameParser;

impl TargetParser for TnameParser {
    type Target = String;

    fn parse_target(v: &ast::Value) -> Result<Self::Target, ParseError> {
        match v {
            ast::Value::Primitive(pv) => match pv {
                ast::Primitive::IdentT(s) => Ok(s.clone()),
                _ => Err(ParseError::new(ErrorKind::Wtf, "not a IdentT")),
            },
            _ => Err(ParseError::new(ErrorKind::Wtf, "not a Primitive value")),
        }
    }
}

impl TargetListParser for TnameParser {
    type TP = Self;
}

impl IterGroup {
    fn from_definition(def: &ast::Definition) -> Result<Self, ParseError> {
        let m = Self::convert(def.list.iter().map(|a| &(**a)))?;

        let messages = m
            .get("messages")
            .ok_or(ParseError::new(ErrorKind::Wtf, "messages must be defined"))
            .and_then(|v| TnameParser::parse_target_list(*v))?;

        Ok(IterGroup {
            tname: def.identt.clone(),
            messages,
        })
    }
}

pub enum TypedDefinition {
    Pkt(Packet),
    Msg(Message),
    Ig(IterGroup),
}

impl TypedDefinition {
    pub fn parse_definition(def: &ast::Definition) -> Result<TypedDefinition, ParseError> {
        match def.deft {
            ast::DefType::Packet => Packet::from_definition(def).map(|def| Self::Pkt(def)),
            ast::DefType::Message => Message::from_definition(def).map(|def| Self::Msg(def)),
            ast::DefType::IterGroup => IterGroup::from_definition(def).map(|def| Self::Ig(def)),
        }
    }
}
