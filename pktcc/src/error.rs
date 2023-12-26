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
