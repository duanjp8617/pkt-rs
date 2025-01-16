use super::{max_value, BuiltinTypes, Error, Header, LengthField};

/// The ast type that are constructed when parsing `cond` of the `message` type.
///
/// `conds` represents a series of equal conditions that combined through the
/// logical or operator.
#[derive(Debug)]
pub struct Cond {
    conds: Vec<(String, u64)>,
}

impl Cond {
    pub fn at(&self, index: usize) -> (&String, &u64) {
        let (field_name, value) = &self.conds[index];
        (field_name, value)
    }

    pub fn from_cond_list(conds: Vec<(String, u64)>, header: &Header) -> Result<Self, Error> {
        // Make sure the following conditions hold:
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

        match conds_iter.next() {
            None => Ok(Cond { conds }),
            Some(first_cond) => {
                // make sure that the first condition passes the check
                cond_checker(first_cond)?;

                // make sure that all following conditions pass the check
                let mut err = None;
                if conds_iter.all(|cond| {
                    if cond.0 != first_cond.0 {
                        err = Some(Error::field(
                            5,
                            format!(
                                "field name {} does not match that in the first condition",
                                cond.0
                            ),
                        ));
                        return false;
                    }
                    match cond_checker(cond) {
                        Ok(_) => true,
                        Err(e) => {
                            err = Some(e);
                            false
                        }
                    }
                }) {
                    Ok(Cond { conds })
                } else {
                    Err(err.unwrap())
                }
            }
        }
    }
}
