use super::{max_value, BuiltinTypes, Error, Header};

/// The ast type that are constructed when parsing `cond` of the `message` type.
///
/// `conds` represents a series of equal conditions that combined through the
/// logical or operator.
#[derive(Debug)]
pub struct Cond {
    field_name: String,
    compared_values: Vec<u64>,
}

impl Cond {
    pub fn field_name(&self) -> &str {
        &self.field_name
    }

    pub fn compared_values(&self) -> &Vec<u64> {
        &self.compared_values
    }

    pub fn from_cond_list(conds: Vec<(String, u64)>, header: &Header) -> Result<Self, Error> {
        // Make sure the following hold:
        // 1. cond contains a valid field name
        // 2. `repr` is u8/u16/u32/u64
        // 3. the compared value in the cond does not exceeds the bit limit of the
        //    field.
        // 4. the field's `gen` is true, meaning that it is not a length-related field.
        let cond_checker = |cond: &(String, u64)| -> Result<(), Error> {
            let (field_name, compared_value) = (&cond.0, &cond.1);
            let (field, _) = header.field(field_name).ok_or(Error::field(
                1,
                format!("invalid field name in cond expression: {field_name}"),
            ))?;

            if field.repr == BuiltinTypes::ByteSlice {
                return_err!(Error::field(
                    2,
                    "field repr can not be a byte slice".to_string()
                ));
            }

            if *compared_value > max_value(field.bit).unwrap() {
                return_err!(Error::field(
                    3,
                    format!("compared value {compared_value} is too large")
                ));
            }

            if !field.gen {
                return_err!(Error::field(4, "field gen must be true".to_string()));
            }

            Ok(())
        };

        let mut conds_iter = conds.iter();

        // The parser ensures that conds list is non-empty.
        let first_cond = conds_iter.next().unwrap();
        // make sure that the first condition passes the check
        cond_checker(first_cond)?;

        let field_name = first_cond.0.clone();
        let mut compared_values = vec![first_cond.1];

        for cond in conds_iter {
            // make sure that each following cond's field name
            // is the same as the first cond
            if cond.0 != first_cond.0 {
                return_err!(Error::field(
                    5,
                    format!(
                        "field name {} does not match that in the first condition",
                        cond.0
                    ),
                ))
            }
            // and that each following cond passes the check
            cond_checker(cond)?;
            compared_values.push(cond.1);
        }

        Ok(Self {
            field_name,
            compared_values,
        })
    }
}
