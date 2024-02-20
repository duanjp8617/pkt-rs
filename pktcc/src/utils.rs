/// Some of the key utilities that are compulsory for a lalrpop-parser

use crate::ast::Error as AstError;
use crate::token::Error as TokenError;

// A special wrapper type for various types needed by lalrpop,
// including tokenizer and the parser.
// The two usize represents the left and right position within the
// text.
// The T represents the token or the parsed type.
pub type Spanned<T> = (usize, T, usize);

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

// A quick way to return an error defined by quick_err macro
macro_rules! return_err {
    ($err_ty: ty, $arm: ident, $($args: expr),+) => {
        return Err(<$err_ty>::$arm($($args),+))
    }
}