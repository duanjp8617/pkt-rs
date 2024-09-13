use std::io::Write;

use crate::ast::{max_value, BitPos, Field, UsableAlgExpr};

use super::{FieldGetMethod, FieldSetMethod, HeadTailWriter};

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
        let func_def = format!("#[inline]\npub fn {length_field_name}(&self)->usize{{\n");
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Here, the checks performed by the parser will ensure that
        // field `arg` is the same as field `repr`, and that `repr` is
        // one of `U8`, `U16`, `U32` and `U64`.
        // We also ensure in the parser that the calculated length will
        // not exceed a pre-defined constant that is way smaller than the
        // maximum value of `USIZE`.
        // So, we read the field value and cast its type to `usize`.
        let mut buf = Vec::new();
        {
            let mut temp_writer = HeadTailWriter::new(&mut buf, "(", ") as usize");
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
        let func_def = format!(
            "#[inline]\npub fn set_{length_field_name}(&mut self, {write_value}:usize){{\n",
        );
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Next, we calculate a series of guard conditions for the header set
        // method.
        let mut guards = Vec::new();

        // Here, the bit size of the length field is guaranteed to be smaller than that
        // of usize. In the meantime, the maximum length that can be calculated derived
        // from the underlying expression is guaranteed to be a value that can fit in
        // usize as well. So here, we just need to make sure that the input argument is
        // smaller than the maximum length and larger than the fixed header length.
        guards.push(format!(
            "{write_value}<={}",
            self.expr.exec(max_value(self.field.bit).unwrap()).unwrap()
        ));
        // guards.push(format!(
        //     "{write_value}>={}",

        // ));


        let guard_str = self.expr.reverse_exec_guard(write_value);
        if guard_str.len() > 0 {
            // This guard condition ensures that the `write_value`
            // can be divided without remainder.
            guards.push(guard_str);
        }

        // If the guard conditions are present, we prepend them to the generated method.
        // TODO: update code gen
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
            let mut val_def_writer = HeadTailWriter::new(
               &mut buf,
                "(",
                &format!(") as {}", self.field.repr.to_string()),
            );
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
