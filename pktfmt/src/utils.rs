/// Some of the key utilities that are compulsory for a lalrpop-parser
use std::io::Write;

use crate::ast::Error as AstError;
use crate::file_text::FileText;
use crate::token::Error as TokenError;

// A special wrapper type for that records the location
// of the contained item
pub struct Spanned<T> {
    pub item: T,
    // span is a non-inclusive range
    pub span: (usize, usize),
}

quick_error! {
    /// A toplevel error type.
    ///
    /// This error type wraps all the errors exposed by various parsing stages.
    /// It is also the argument to the `E` type parameter in `lalrpop_util::ParseError`.
    ///
    /// During parsing, all the errors generated will first be converted to the toplevel
    /// `Error` type, and then parsed to the lalrpop parser as the an argument of the
    /// `lalrpop_util::ParseError::User { error }`.
    #[derive(Debug, Eq, PartialEq, Clone)]
    pub enum Error {
        Token(err: TokenError) {
            display("{}", err)
            from()
        }
        Ast{err: AstError, span: (usize, usize)} {
            display("{}", err)
        }
        Lalrpop(err_str: String) {
            display("{}", err_str)
        }
    }
}

#[macro_export]
// A macro that drives the parser.
// The returned error is converted into the toplevel error type.
macro_rules! parse_with_error {
    ($parser: ty, $tokenizer: expr, $($parser_args: expr),*) => {
        <$parser>::new()
        .parse(
            $($parser_args),*,
            $tokenizer
                .into_iter()
                .map(|tk_res| tk_res.map_err(|err| crate::utils::Error::Token(err))),
        )
        .map_err(|err| match err {
            ::lalrpop_util::ParseError::User { error } => error,
            _ => crate::utils::Error::Lalrpop(format!("{}", err)),
        })
    }
}

/// Render the error message in the `out` writer.
/// The detailed position of the `error` is rendered through the `file_text`.
///
/// We first print a summary of the error.
/// Then we render the code block that generate the error.
/// We finalize the printing with the detailed explanation.
pub fn render_error(file_text: &FileText, error: Error, out: &mut dyn Write) {
    // print error summary, then render error location
    match error {
        Error::Token(ref err) => {
            writeln!(out, "error: token error").unwrap();

            file_text
                .render_code_block(err.location, err.location, out)
                .unwrap();
        }
        Error::Ast { ref err, ref span } => {
            write!(out, "error: ").unwrap();
            match err {
                AstError::InvalidField(_) => writeln!(out, "invalid Field definition").unwrap(),
                AstError::InvalidHeader(_) => writeln!(out, "invalid Header definition").unwrap(),
            };

            file_text
                .render_code_block(span.0, span.1 - 1, out)
                .unwrap();
        }
        Error::Lalrpop(_) => {
            writeln!(out, "error: lalrpop error").unwrap();
        }
    }
    // print error details
    writeln!(out, "note: {}", error).unwrap();
}

// A quick way to return an error defined by quick_err macro
macro_rules! return_err {
    ($err_ty: ty, $arm: ident, $($args: expr),+) => {
        return Err(<$err_ty>::$arm($($args),+))
    }
}
