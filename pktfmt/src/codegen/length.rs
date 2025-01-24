use std::io::Write;

use crate::ast::{
    max_value, BitPos, BuiltinTypes, DefaultVal, Field, Header, Length, LengthField, UsableAlgExpr,
};

use super::{FieldGetMethod, FieldSetMethod, HeadTailWriter};

pub struct LengthGenerator<'a> {
    header: &'a Header,
    length: &'a Length,
}

impl<'a> LengthGenerator<'a> {
    const LENGTH_FIELD_NAMES: &'static [&'static str] =
        &["header_len", "payload_len", "packet_len"];

    pub fn new(header: &'a Header, length: &'a Length) -> Self {
        Self { header, length }
    }

    pub fn code_gen(&self, target_slice: &str, write_value: Option<&str>, output: &mut dyn Write) {
        for index in 0..3 {
            match self.length.at(index) {
                LengthField::Expr { expr } => {
                    let (field, start) = self.header.field(expr.field_name()).unwrap();
                    match write_value {
                        Some(write_value) => {
                            LengthSetMethod::new(field, start, expr).code_gen(
                                Self::LENGTH_FIELD_NAMES[index],
                                target_slice,
                                write_value,
                                output,
                            );
                        }
                        None => {
                            LengthGetMethod::new(field, start, expr).code_gen(
                                Self::LENGTH_FIELD_NAMES[index],
                                target_slice,
                                output,
                            );
                        }
                    }
                }
                _ => {} // do nothing
            }
        }
    }
}

/// A helper object that generate length get method for length field.
pub struct LengthGetMethod<'a> {
    field: &'a Field,
    start: BitPos,
    expr: &'a UsableAlgExpr,
}

impl<'a> LengthGetMethod<'a> {
    pub fn new(field: &'a Field, start: BitPos, expr: &'a UsableAlgExpr) -> Self {
        Self { field, start, expr }
    }

    /// Generate a get method to access the length field with name
    /// `length_field_name` from the buffer slice `target_slice`.
    /// The generated method is written to `output`.
    pub fn code_gen(
        &self,
        length_field_name: &str,
        target_slice: &str,
        mut output: &mut dyn Write,
    ) {
        // Generate function definition for a length field get method.
        // It will generate:
        // pub fn length_field_name(&self) -> usize {
        // ...
        // }
        let return_type = length_access_method_io_type(self.expr, self.field);
        let func_def = format!(
            "#[inline]\npub fn {length_field_name}(&self)->{}{{\n",
            return_type.to_string()
        );
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Here, the checks performed by the parser will ensure that
        // field `arg` is the same as field `repr`, and that `repr` is
        // one of `U8`, `U16`, `U32` and `U64`.
        // We also ensure in the parser that the calculated length will
        // not exceed a pre-defined constant that is way smaller than the
        // maximum value of `USIZE`.
        // So, we read the field value and cast its type to the corresponding return
        // type.
        let mut buf = Vec::new();
        {
            let mut temp_writer = if return_type == self.field.repr {
                HeadTailWriter::new(&mut buf, "(", ")")
            } else {
                HeadTailWriter::new(&mut buf, "(", &format!(") as {}", return_type.to_string()))
            };
            FieldGetMethod::new(self.field, self.start)
                .read_repr(target_slice, temp_writer.get_writer());
        }

        // The parser will make sure that evaluating the expression
        // will not lead to any overflow.
        self.expr.gen_exec(
            std::str::from_utf8(&buf[..]).unwrap(),
            func_def_writer.get_writer(),
        );
    }
}

/// A helper object that generate length set method for length field.
pub struct LengthSetMethod<'a> {
    field: &'a Field,
    start: BitPos,
    expr: &'a UsableAlgExpr,
}

impl<'a> LengthSetMethod<'a> {
    pub fn new(field: &'a Field, start: BitPos, expr: &'a UsableAlgExpr) -> Self {
        Self { field, start, expr }
    }

    /// Generate a set method for the length field with name
    /// `length_field_name`. The method will set the length value stored in
    /// `write_value` to the field area stored in `target_slice`.
    /// The generated method is written to `output`.
    pub fn code_gen(
        &self,
        length_field_name: &str,
        target_slice: &str,
        write_value: &str,
        mut output: &mut dyn Write,
    ) {
        // Generate function definition for a length field get method.
        // It will generate:
        // pub fn set_length_field_name(&mut self, write_value: usize) {
        // ...
        // }
        let arg_type = length_access_method_io_type(self.expr, self.field);
        let func_def = format!(
            "#[inline]\npub fn set_{length_field_name}(&mut self, {write_value}:{}){{\n",
            arg_type.to_string()
        );
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Next, we calculate a series of guard conditions for the header set
        // method.
        let mut guards = Vec::new();
        if self.field.default_fix {
            let default_val = match self.field.default {
                DefaultVal::Num(n) => n,
                _ => panic!(),
            };
            let fixed_length = self.expr.exec(default_val).unwrap();
            guards.push(format!("{write_value}=={fixed_length}"));
        } else {
            // we use a closure to check whether the max value of the input arg type is the
            // same as the max value of the length field. If these two are not the same, we
            // add a guard.
            let need_max_length_guard = |max_length: u64| match &arg_type {
                BuiltinTypes::U8 => max_length < u8::MAX as u64,
                BuiltinTypes::U16 => max_length < u16::MAX as u64,
                BuiltinTypes::U32 => max_length < u32::MAX as u64,
                BuiltinTypes::U64 => max_length < u64::MAX as u64,
                _ => panic!(),
            };
            let max_length = self.expr.exec(max_value(self.field.bit).unwrap()).unwrap();
            if need_max_length_guard(max_length) {
                guards.push(format!("{write_value}<={max_length}"));
            }
        }

        let guard_str = self.expr.reverse_exec_guard(write_value);
        if guard_str.len() > 0 {
            // This guard condition ensures that the `write_value`
            // can be divided without remainder.
            guards.push(guard_str);
        }

        // If the guard conditions are present, we prepend them to the generated method.
        if guards.len() > 0 {
            let mut assert_writer =
                HeadTailWriter::new(func_def_writer.get_writer(), "assert!(", ");\n");
            guards.iter().enumerate().for_each(|(idx, s)| {
                write!(assert_writer.get_writer(), "({s})").unwrap();
                if idx < guards.len() - 1 {
                    write!(assert_writer.get_writer(), "&&").unwrap();
                }
            });
        }

        let mut buf = Vec::new();
        {
            // Perform a reverse calculation of the expression,
            // and assign the result to a new local variable.
            let mut val_def_writer = if arg_type == self.field.repr {
                HeadTailWriter::new(&mut buf, "(", ")")
            } else {
                HeadTailWriter::new(
                    &mut buf,
                    "((",
                    &format!(") as {})", self.field.repr.to_string()),
                )
            };
            self.expr
                .gen_reverse_exec(write_value, val_def_writer.get_writer());
        }

        // Finally, set the new local variable containing the field value
        // to the field area on the `target_slice`.
        FieldSetMethod::new(self.field, self.start).write_repr(
            target_slice,
            std::str::from_utf8(&buf[..]).unwrap(),
            func_def_writer.get_writer(),
        );
    }
}

// Find out the input and output types of a length field.
fn length_access_method_io_type(expr: &UsableAlgExpr, field: &Field) -> BuiltinTypes {
    let max_length = expr.exec(max_value(field.bit).unwrap()).unwrap();
    if max_length <= (u8::MAX as u64) {
        BuiltinTypes::U8
    } else if max_length <= (u16::MAX as u64) {
        BuiltinTypes::U16
    } else if max_length <= (u32::MAX as u64) {
        BuiltinTypes::U32
    } else {
        BuiltinTypes::U64
    }
}
