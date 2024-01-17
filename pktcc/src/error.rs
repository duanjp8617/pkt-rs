#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
    kind: ErrorKind,
    msg: String,
}

impl ParseError {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn new(kind: ErrorKind, msg: &str) -> Self {
        Self {
            kind,
            msg: msg.to_string(),
        }
    }
}

#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    CtorType,
    CtorAssignIdent,
    CtorAssignValue,
    Wtf,
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}", &self.kind, &self.msg)
    }
}

use quick_error::ResultExt;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        File(filename: PathBuf, err: io::Error) {
            context(path: &'a Path, err: io::Error)
                -> (path.to_path_buf(), err)
        }
    }
}

fn openfile(path: &Path) -> Result<(), Error> {
    File::open(path).context(path)?;

    // If we didn't have context, the line above would be written as;
    //
    // File::open(path)
    //     .map_err(|err| Error::File(path.to_path_buf(), err))?;

    Ok(())
}

#[test]
fn test_char() {
    let c: char = 'h';
    let s = "wtffff你是傻逼";
    println!("{}", s.len());
    let sub_s = &s[15..18];
    let c = sub_s.chars().next().unwrap();
    println!("{}, sub_s char boundary {}", c, sub_s.is_char_boundary(4));
    for (index, c) in s.char_indices() {
        println!("index: {}, char: {}, char len: {}", index, c, c.len_utf8());
    }
    println!("{}", c.len_utf8());
}
