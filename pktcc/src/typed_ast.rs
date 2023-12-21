use std::collections::HashMap;

use crate::ast;
use crate::error::{ErrorKind, ParseError};

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Arg {
    BuiltinType(ast::BuiltinType),
    RsType(String),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum DefaultVal {
    Num(u64),
    Bool(bool),
    ByteArray(Vec<u8>),
    RsExpr(String),
}

pub struct Field {
    bit: u64,
    repr: ast::BuiltinType,
    arg: Arg,
    default: DefaultVal,
    gen: bool,
}

impl Field {
    pub fn from_ctor(v: &ast::Ctor) -> Result<Field, ParseError> {
        if v.identt != "Field" {
            return Err(ParseError::new(ErrorKind::CtorType, &v.identt));
        }

        let mut m = HashMap::new();
        for (identv, v) in v
            .list
            .iter()
            .map(|assign| (assign.identv.as_str(), &*assign.value))
        {
            // make sure the assigned identifier belong to 1 of the 5:
            // bit, repr, arg, default, gen
            if !Self::identifier_valid(identv) {
                return Err(ParseError::new(
                    ErrorKind::AssignIdent,
                    &format!("invalid identifier {}", identv),
                ));
            }

            // ensure the assigned value is a primitive one
            let pv = match v {
                ast::Value::Primitive(pv) => Ok(pv),
                _ => Err(ParseError::new(
                    ErrorKind::AssignValue,
                    "not primitive value",
                )),
            }?;

            // prevent duplicated identifiers
            m.insert(identv, pv).map_or(Ok(()), |_| {
                Err(ParseError::new(
                    ErrorKind::AssignIdent,
                    &format!("duplicated identifier {}", identv),
                ))
            })?;
        }

        // 1. bit must be defined
        // 2. bit size should not be 0,
        // 3. if bit size is larger than 64, it should be dividable by 8
        let bit = m
            .get("bit")
            .ok_or(ParseError::new(ErrorKind::AssignIdent, "missing bit"))
            .and_then(|pv| {
                pv.try_take_num()
                    .ok_or(ParseError::new(
                        ErrorKind::AssignValue,
                        "value is not a number",
                    ))
                    .and_then(|n| {
                        if n == 0 {
                            Err(ParseError::new(ErrorKind::AssignValue, "bit cannot be 0"))
                        } else if n > 64 && n % 8 != 0 {
                            Err(ParseError::new(ErrorKind::AssignValue, "invalid bit size"))
                        } else {
                            Ok(n)
                        }
                    })
            })?;

        let repr = match m.get("repr") {
            Some(v) => match v {
                ast::Primitive::BuiltinType(defined) => {
                    let inferred = ast::BuiltinType::infer_from_bit_size(bit);

                    if inferred == *defined {
                        // OK: defined repr is the same as the inferred repr
                        *defined
                    } else if *defined == ast::BuiltinType::ByteSlice && bit % 8 == 0 {
                        // OK: use &[u8] to override the inferred repr
                        *defined
                    } else {
                        // Err: defined repr does not match inferred repr
                        return Err(ParseError::new(ErrorKind::AssignValue, "invalid repr"));
                    }
                }
                _ => {
                    // Err: value of repr must be a BuiltinType
                    return Err(ParseError::new(
                        ErrorKind::AssignValue,
                        "repr should be a BuiltinType",
                    ));
                }
            },
            None => {
                // if repr is not defined, repr is inferred from bit
                ast::BuiltinType::infer_from_bit_size(bit)
            }
        };

        let arg = match m.get("arg") {
            Some(v) => match v {
                ast::Primitive::BuiltinType(defined) => {
                    if *defined == repr {
                        // Ok: defined arg is the same as the repr
                        Arg::BuiltinType(*defined)
                    } else if *defined == ast::BuiltinType::Bool && bit == 1 {
                        // Ok: defined arg is bool while bit size is 1
                        Arg::BuiltinType(*defined)
                    } else {
                        // Err: defined arg does not match repr
                        return Err(ParseError::new(ErrorKind::AssignValue, "invalid repr"));
                    }
                }
                ast::Primitive::IdentT(rs_type) => {
                    // Ok: if arg is a type identifier, then we treat it as a regular rust type
                    Arg::RsType(rs_type.clone())
                }
                _ => {
                    // Err: value of arg is incorrect
                    return Err(ParseError::new(
                        ErrorKind::AssignValue,
                        "arg should be a BuiltinType or a IdentT",
                    ));
                }
            },
            None => {
                // if arg is not defined, it is the same as the repr
                Arg::BuiltinType(repr)
            }
        };

        let default = match m.get("default") {
            Some(v) => match &arg {
                Arg::BuiltinType(bt) => {
                    if *bt == ast::BuiltinType::Bool {
                        let b = v.try_take_bool_val().ok_or(ParseError::new(
                            ErrorKind::AssignValue,
                            "default should be a number",
                        ))?;
                        DefaultVal::Bool(b)
                    } else if *bt == ast::BuiltinType::ByteSlice {
                        match v {
                            ast::Primitive::RsExpr(s) => DefaultVal::RsExpr(s.clone()),
                            _ => {
                                return Err(ParseError::new(
                                    ErrorKind::AssignValue,
                                    "default should be an rust expression",
                                ));
                            }
                        }
                    } else {
                        let n = v.try_take_num().ok_or(ParseError::new(
                            ErrorKind::AssignValue,
                            "default should be a number",
                        ))?;
                        DefaultVal::Num(n)
                    }
                }
                Arg::RsType(_) => match v {
                    ast::Primitive::RsExpr(s) => DefaultVal::RsExpr(s.clone()),
                    _ => {
                        return Err(ParseError::new(
                            ErrorKind::AssignValue,
                            "default should be an rust expression",
                        ));
                    }
                },
            },
            None => {
                // if arg is not defined, it is the same as the repr
                match &arg {
                    Arg::BuiltinType(bt) => {
                        if *bt == ast::BuiltinType::Bool {
                            DefaultVal::Bool(bool::default())
                        } else if *bt == ast::BuiltinType::ByteSlice {
                            let mut v = Vec::new();
                            (0..(bit / 8) as usize).for_each(|_| v.push(u8::default()));
                            DefaultVal::ByteArray(v)
                        } else {
                            DefaultVal::Num(u64::default())
                        }
                    }
                    Arg::RsType(rs) => {
                        return Err(ParseError::new(
                            ErrorKind::AssignValue,
                            "can not infer value",
                        ))
                    }
                }
            }
        };

        let gen = match m.get("gen") {
            Some(v) => v.try_take_bool_val().ok_or(ParseError::new(
                ErrorKind::AssignValue,
                "gem should be a bool",
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

    fn identifier_valid(s: &str) -> bool {
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

// pub struct VarField {
//     len: OpExpr,
//     item_size: Option<u64>,
//     arg: Option<String>,
// }

// pub struct LenField {
//     t: LenType,
//     expr: OpExpr,
//     min: u64,
//     max: u64,
// }

// pub struct Packet {
//     header: Vec<Field>,
//     var_header: Vec<VarField>,
//     with_payload: bool,
//     header_len: Option<LenField>,
//     payload_len: Option<LenField>,
//     packet_len: Option<LenField>,
// }

// pub struct Message {
//     header: Vec<Field>,
//     var_header: Vec<VarField>,
//     header_len: Option<LenField>,
//     cond: Option<BinopExpr>
// }
