/// Some of the key utilities that are compulsory for a lalrpop-parser
use std::io::{Read, Write};

use lalrpop_util::ParseError;

use crate::ast::Error as AstError;
use crate::file_text::FileText;
use crate::token::Error as TokenError;

// A special wrapper type for that records the location
// of the contained item
pub struct Spanned<T> {
    item: T,
    span: (usize, usize),
}

// A public error type, that is used by both the
// tokenizer and the parser.
quick_error! {
    #[derive(Debug, Eq, PartialEq, Clone)]
    pub enum Error {
        Token(err: TokenError) {
            display("{}", err)
            from()
        }
        Ast{err: AstError, span: (usize, usize)} {
            display("{}", err)
        }
    }
}

pub fn render_error<L, T>(file_text: &FileText, error: ParseError<L, T, Error>)
where
    L: std::fmt::Display,
    T: std::fmt::Display,
{
    let mut std_err = std::io::stderr();
    match error {
        ParseError::User { error } => match error {
            Error::Token(ref err) => {
                file_text
                    .render_code_block(err.location, err.location, &mut std_err)
                    .unwrap();
                write!(std_err, "{}", err).unwrap();
            }
            Error::Ast { ref err, ref span } => {
                file_text
                    .render_code_block(span.0, span.1 - 1, &mut std_err)
                    .unwrap();

                write!(std_err, "{}", err).unwrap();
            }
        },
        _ => {
            write!(std_err, "{}", error).unwrap();
        }
    }
    writeln!(std_err, "").unwrap();
}

// A quick way to return an error defined by quick_err macro
macro_rules! return_err {
    ($err_ty: ty, $arm: ident, $($args: expr),+) => {
        return Err(<$err_ty>::$arm($($args),+))
    }
}
