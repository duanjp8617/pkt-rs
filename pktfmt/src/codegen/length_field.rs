use std::io::Write;

use crate::ast::{BitPos, Field, UsableAlgExpr};

use super::{FieldGetMethod, FieldSetMethod, HeadTailWriter};

pub(crate) struct LengthGetMethod<'a> {
    field: &'a Field,
    start: BitPos,
    expr: &'a UsableAlgExpr,
}

impl<'a> LengthGetMethod<'a> {
    // Generate a get method to access the length field with name
    // `length_field_name` from the buffer slice `target_slice`.
    // The generated method is written to `output`.
    fn code_gen(&self, length_field_name: &str, target_slice: &str, mut output: &mut dyn Write) {
        // Generate function definition for a length field get method.
        // It will generate:
        // pub fn length_field_name(&self) -> usize {
        // ...
        // }
        let func_def = format!("#[inline]\npub fn {length_field_name}(&self)->usize{{\n");
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Here, the checks performed by the parser will ensure that
        // field `arg` is the same as field `repr`, and that `repr` is
        // one of `U8`, `U16`, `U32` and `U64`.
        // We also ensure in the parser that the calculated length will
        // not exceed a pre-defined constant that is way smaller than the
        // maximum value of `USIZE`.
        // So, we first read the field value into a local variable `length_val`,
        // and cast its type to `usize`.
        {
            let mut temp_writer = HeadTailWriter::new(
                func_def_writer.get_writer(),
                "let length_val=(",
                ") as usize;\n",
            );
            FieldGetMethod::new(self.field, self.start)
                .read_repr(target_slice, temp_writer.get_writer());
        }

        // The parser will make sure that evaluating the expression
        // will not lead to any overflow.
        self.expr
            .gen_exec("length_val", func_def_writer.get_writer());
    }
}

struct LengthSetMethod<'a> {
    field: &'a Field,
    start: BitPos,
    expr: &'a UsableAlgExpr,
}

impl<'a> LengthSetMethod<'a> {
    // Generate a set method for the length field with name `length_field_name`.
    // The method will set the length value stored in `write_value` to the field
    // area stored in `target_slice`.
    // The generated method is written to `output`.
    fn code_gen(
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
        let func_def = format!(
            "#[inline]\npub fn set_{length_field_name}(&mut self, {write_value}:usize){{\n",
        );
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Next, we calculate a series of guard conditions for the header set
        // method.
        let mut guards = Vec::new();

        /* Here, the `USIZE_BYTES` is the total byte number of the `usize` type.
        `usize` is the default argument type for all the length methods, this
        is designed to facilitate length calculation in rust.

        In the parser, we will make sure that, for the field that is used for
        length calculation, its bit size should not exceed `USIZE_BYTES * 8`.
        We will have two branches depending on the field bit size:

        1st, if field bit size equals `USIZE_BYTES * 8`, then the only possible
        way to calculate the maximum length without triggering overflow is to
        directly return the field value. In this case, we don't need any form of
        guard condition for the length set method.

        2nd, if the bit size is smaller than `USIZE_BYTES * 8`, we need to make sure that
        the input value to the header set method is smaller than the maximum expression value
        produced by the maximum field value. */
        if self.field.bit < crate::USIZE_BYTES * 8 {
            // This guard condition corresponds to the 2nd branch.
            guards.push(format!(
                "{write_value}<={}",
                self.expr
                    .exec((2 as u64).pow(self.field.bit as u32) - 1)
                    .unwrap()
            ));
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

        {
            // Perform a reverse calculation of the expression,
            // and assign the result to a new local variable.
            let mut val_def_writer = HeadTailWriter::new(
                func_def_writer.get_writer(),
                "let field_val = (",
                &format!(") as {};\n", self.field.repr.to_string()),
            );
            self.expr
                .gen_reverse_exec(write_value, val_def_writer.get_writer());
        }

        // Finally, set the new local variable containing the field value
        // to the field area on the `target_slice`.
        FieldSetMethod::new(self.field, self.start).write_repr(
            target_slice,
            "field_val",
            func_def_writer.get_writer(),
        );
    }
}
