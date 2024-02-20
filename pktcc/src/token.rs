use crate::utils::Spanned;

// The error type for the tokenizer is taken from lalrpop.
// The error type needs to remember the starting position of the erroneous token
// as well as the error code to facilitate error reporting.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: usize,
    pub code: ErrorCode,
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.code {
            ErrorCode::InvalidToken => write!(fmt, "invalid token"),
            ErrorCode::UnclosedCodeSegment => write!(fmt, "uncolosed code segment"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    InvalidToken,
    UnclosedCodeSegment,
}

fn error<T>(c: ErrorCode, l: usize) -> Result<T, Error> {
    Err(Error {
        location: l,
        code: c,
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'input> {
    // top-level keyword
    Packet,
    Message,
    IterGroup,

    // packet keyword
    Header,
    VarHeader,
    WithPayload,
    HeaderLen,
    PayloadLen,
    PacketLen,

    // message keyword
    Cond,

    // iter group keyword
    Messages,

    // Field keyword
    Field,
    Bit,
    Repr,
    Arg,
    Default,
    Gen,

    // VarField keyword
    VarField,
    Len,
    ItemSize,

    // HeaderField keyworld
    Expr,
    Min,
    Max,

    // Identifiers
    Ident(&'input str),

    // Builtin Type
    BuiltinType(&'input str),

    // Boolean value, true, false
    BooleanValue(&'input str),

    // Comparison
    Eq,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,

    // Logical
    Not,
    And,
    Or,

    // Algorithic
    Plus,
    Minus,
    Mult,
    Div,

    // Brackets
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Comma
    Comma,

    // Assign
    Assign,

    // Numbers
    Num(&'input str),

    // rust code enclosed by %%:  %%RsType::new(1)%%
    Code(&'input str),

    // doc
    // /// doc string
    // /// doc string
    Doc(&'input str),
}

// lalrpop ParseError only implements std::fmt::Display if
// L, T and E all implement std::fmt::Display.
// So it's better to implement Display for the Token.
impl<'input> std::fmt::Display for Token<'input> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

const KEYWORDS: &[(&str, Token)] = &[
    ("packet", Token::Packet),
    ("message", Token::Message),
    ("iter_group", Token::IterGroup),
    ("header", Token::Header),
    ("var_header", Token::VarHeader),
    ("with_payload", Token::WithPayload),
    ("header_len", Token::HeaderLen),
    ("payload_len", Token::PayloadLen),
    ("packet_len", Token::PacketLen),
    ("cond", Token::Cond),
    ("messages", Token::Messages),
    ("Field", Token::Field),
    ("bit", Token::Bit),
    ("repr", Token::Repr),
    ("arg", Token::Arg),
    ("default", Token::Default),
    ("gen", Token::Gen),
    ("VarField", Token::VarField),
    ("len", Token::Len),
    ("item_size", Token::ItemSize),
    ("expr", Token::Expr),
    ("min", Token::Min),
    ("max", Token::Max),
];

const BUILTIN_TYPES: &[&str] = &["u8", "u16", "u32", "u64", "bool"];

const BOOLEAN_VALUES: &[&str] = &["true", "false"];

pub struct Tokenizer<'input> {
    text: &'input str,
    // the input char stream
    chars: std::str::CharIndices<'input>,
    // the current head of the stream
    head: Option<(usize, char)>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(text: &'input str) -> Self {
        let mut chars = text.char_indices();
        let head = chars.next();

        Self { text, chars, head }
    }

    // Peek the stream head.
    fn peek(&self) -> Option<(usize, char)> {
        self.head
    }

    // Consume the current stream head, and return the next stream head.
    fn consume_and_peek(&mut self) -> Option<(usize, char)> {
        self.head = self.chars.next();
        self.head
    }

    fn take_while<F>(&mut self, mut f: F) -> Option<(usize, char)>
    where
        F: FnMut(char) -> bool,
    {
        while let Some((_, c)) = self.peek() {
            if f(c) {
                self.consume_and_peek();
            } else {
                break;
            }
        }

        self.peek()
    }

    fn take_until<F>(&mut self, mut f: F) -> Option<(usize, char)>
    where
        F: FnMut(char) -> bool,
    {
        self.take_while(|c| !f(c))
    }

    // Check whether the current stream head is the last
    // character of the stream.
    fn is_last_char(&self) -> bool {
        self.head
            .map(|_| {
                let next_opt = self.chars.clone().next();
                next_opt.map(|_| false).unwrap_or(true)
            })
            .unwrap_or(false)
    }

    // Match the stream agaist the input word, until the
    // last character of the word is found.
    // Return the byteoffset of the last character of a word.
    // This method does not consume the last character from the input stream.
    fn match_word(&mut self, word: &str) -> Option<usize> {
        // make sure that the input word is not empty
        assert!(word.len() > 0);

        let mut word_chars = word.chars().peekable();
        let mut match_last = false;
        self.take_while(|c| {
            let expected = word_chars.next().unwrap();

            if expected != c {
                return false;
            } else {
                match word_chars.peek() {
                    Some(_) => {
                        return true;
                    }
                    None => {
                        match_last = true;
                        return false;
                    }
                }
            }
        });

        if match_last {
            self.peek().map(|(offset, _)| offset)
        } else {
            None
        }
    }

    // Search for the last character of a code segment.
    // A code segment looks like "%%<code...>%%".
    // It will make the stream stay on the closing '%' symbol.
    fn search_for_end_of_code_segment(&mut self) -> Option<usize> {
        loop {
            self.take_until(|c| c == '%')?;
            match self.consume_and_peek() {
                Some((idx, '%')) => {
                    return Some(idx);
                }
                _ => {
                    continue;
                }
            }
        }
    }

    // Search for a complete doc line.
    // It ends either on the first '\n' character, or the last character
    // of the stream.
    // It returns the byte offset of the ending character.
    fn search_for_doc_line(&mut self) -> Option<usize> {
        match self.match_word("///") {
            Some(_) => loop {
                match self.peek() {
                    Some((idx, '\n')) => return Some(idx),
                    Some((idx, _)) if self.is_last_char() => return Some(idx),
                    _ => {
                        self.consume_and_peek();
                    }
                }
            },
            None => None,
        }
    }

    // Given an initial doc line ended at the 'ending_pos',
    // search for all the subsequent doc lines.
    // We first consume the whitespaces after the current doc line.
    // Then we try to consume the next doc line. If we succeed, we update
    // the 'ending_pos' to point to the last character of the new doc line.
    // If we fail, the caller can recover position of the ending doc line
    // using 'ending_pos'.
    fn search_for_subsequent_docs(
        &mut self,
        ending_pos: &mut (Option<(usize, char)>, std::str::CharIndices<'input>),
    ) {
        loop {
            match self.take_while(|c| c.is_whitespace()) {
                Some((_, '/')) => match self.search_for_doc_line() {
                    Some(_) => {
                        ending_pos.0 = self.head;
                        ending_pos.1 = self.chars.clone();
                    }
                    None => break,
                },
                _ => break,
            }
        }
    }

    fn next_token(&mut self) -> Option<Result<(usize, Token<'input>, usize), Error>> {
        loop {
            match self.peek() {
                Some((idx, '+')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::Plus, idx + 1)));
                }
                Some((idx, '-')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::Minus, idx + 1)));
                }
                Some((idx, '*')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::Mult, idx + 1)));
                }
                Some((idx, '(')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::LParen, idx + 1)));
                }
                Some((idx, ')')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::RParen, idx + 1)));
                }
                Some((idx, '{')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::LBrace, idx + 1)));
                }
                Some((idx, '}')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::RBrace, idx + 1)));
                }
                Some((idx, '[')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::LBracket, idx + 1)));
                }
                Some((idx, ']')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::RBracket, idx + 1)));
                }
                Some((idx, ',')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::Comma, idx + 1)));
                }
                Some((idx, '=')) => match self.consume_and_peek() {
                    Some((next_idx, '=')) => {
                        self.consume_and_peek();
                        return Some(Ok((idx, Token::Eq, next_idx + 1)));
                    }
                    _ => {
                        return Some(Ok((idx, Token::Assign, idx + 1)));
                    }
                },
                Some((idx, '!')) => match self.consume_and_peek() {
                    Some((next_idx, '=')) => {
                        self.consume_and_peek();
                        return Some(Ok((idx, Token::Neq, next_idx + 1)));
                    }
                    _ => {
                        return Some(Ok((idx, Token::Not, idx + 1)));
                    }
                },
                Some((idx, '>')) => match self.consume_and_peek() {
                    Some((next_idx, '=')) => {
                        self.consume_and_peek();
                        return Some(Ok((idx, Token::Ge, next_idx + 1)));
                    }
                    _ => {
                        return Some(Ok((idx, Token::Gt, idx + 1)));
                    }
                },
                Some((idx, '<')) => match self.consume_and_peek() {
                    Some((next_idx, '=')) => {
                        self.consume_and_peek();
                        return Some(Ok((idx, Token::Le, next_idx + 1)));
                    }
                    _ => {
                        return Some(Ok((idx, Token::Lt, idx + 1)));
                    }
                },
                Some((idx, '|')) => match self.consume_and_peek() {
                    Some((next_idx, '|')) => {
                        self.consume_and_peek();
                        return Some(Ok((idx, Token::Or, next_idx + 1)));
                    }
                    _ => {
                        continue;
                    }
                },
                Some((idx, '/')) => match self.consume_and_peek() {
                    Some((_, '/')) => match self.consume_and_peek() {
                        Some((_, '/')) => match self.take_until(|c| c == '\n') {
                            Some(_) => {
                                let mut ending_pos = (self.head, self.chars.clone());
                                self.search_for_subsequent_docs(&mut ending_pos);

                                // do consume and peek manually
                                let next_idx = ending_pos.0.unwrap().0;
                                self.head = ending_pos.1.next();
                                self.chars = ending_pos.1;

                                return Some(Ok((
                                    idx,
                                    Token::Doc(&self.text[idx..next_idx + 1]),
                                    next_idx + 1,
                                )));
                            }
                            _ => {
                                return Some(Ok((
                                    idx,
                                    Token::Doc(&self.text[idx..]),
                                    self.text.len(),
                                )))
                            }
                        },
                        // The next arm handles single line comment starting
                        // with "//"
                        _ => match self.take_until(|c| c == '\n') {
                            Some(_) => {
                                self.consume_and_peek();
                                continue;
                            }
                            _ => continue,
                        },
                    },
                    _ => return Some(Ok((idx, Token::Div, idx + 1))),
                },
                Some((idx, '&')) => match self.consume_and_peek() {
                    Some((next_idx, '&')) => {
                        self.consume_and_peek();
                        return Some(Ok((idx, Token::And, next_idx + 1)));
                    }
                    _ => match self.match_word("[u8]") {
                        Some(next_idx) => {
                            self.consume_and_peek();
                            return Some(Ok((
                                idx,
                                Token::BuiltinType(&self.text[idx..next_idx + 1]),
                                next_idx + 1,
                            )));
                        }
                        None => {
                            return Some(error(ErrorCode::InvalidToken, idx));
                        }
                    },
                },
                Some((idx, '%')) => match self.consume_and_peek() {
                    Some((_, '%')) => {
                        return self
                            .search_for_end_of_code_segment()
                            .map(|next_idx| {
                                self.consume_and_peek();
                                Some(Ok((
                                    idx,
                                    Token::Code(&self.text[idx..next_idx + 1]),
                                    next_idx + 1,
                                )))
                            })
                            .unwrap_or(Some(error(ErrorCode::UnclosedCodeSegment, idx)));
                    }
                    _ => {
                        return Some(error(ErrorCode::InvalidToken, idx));
                    }
                },
                Some((idx, c)) if identifier_start(c) => {
                    let next_idx = self
                        .take_while(identifier_follow_up)
                        .map(|(next_idx, _)| next_idx)
                        .unwrap_or(self.text.len());

                    let token_str = &self.text[idx..next_idx];
                    let tok = KEYWORDS
                        .iter()
                        .find(|(keyword, _)| *keyword == token_str)
                        .map(|(_, tok)| tok.clone())
                        .or_else(|| {
                            BUILTIN_TYPES
                                .iter()
                                .find(|keyword| **keyword == token_str)
                                .map(|_| Token::BuiltinType(token_str))
                        })
                        .or_else(|| {
                            BOOLEAN_VALUES
                                .iter()
                                .find(|keyword| **keyword == token_str)
                                .map(|_| Token::BooleanValue(token_str))
                        })
                        .unwrap_or(Token::Ident(token_str));
                    return Some(Ok((idx, tok, next_idx)));
                }
                Some((idx, c)) if num_start(c) => {
                    let next_idx = self
                        .take_while(identifier_follow_up)
                        .map(|(next_idx, _)| next_idx)
                        .unwrap_or(self.text.len());

                    let token_str = &self.text[idx..next_idx];
                    return Some(Ok((idx, Token::Num(token_str), next_idx)));
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.take_while(|c| c.is_whitespace());
                    continue;
                }
                Some((idx, _)) => {
                    return Some(error(ErrorCode::InvalidToken, idx));
                }

                None => return None,
            }
        }
    }
}

fn identifier_start(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_')
}

fn identifier_follow_up(c: char) -> bool {
    identifier_start(c) || num_start(c)
}

fn num_start(c: char) -> bool {
    '0' <= c && c <= '9'
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<(usize, Token<'input>, usize), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn test(input: &str, expected: Vec<(&str, Token)>) {
        let tokenizer = Tokenizer::new(&input);
        let len = expected.len();
        for (token, (expected_span, expectation)) in tokenizer.zip(expected.into_iter()) {
            let expected_start = expected_span.find('~').unwrap();
            let expected_end = expected_span.rfind('~').unwrap() + 1;
            println!("token: {:?}", token);

            assert_eq!(Ok((expected_start, expectation, expected_end)), token);
        }

        let mut tokenizer = Tokenizer::new(&input);
        assert_eq!(None, tokenizer.nth(len));
    }

    #[test]
    fn algorithmic1() {
        test(
            r#"+ - * /"#,
            vec![
                (r#"~      "#, Token::Plus),
                (r#"  ~    "#, Token::Minus),
                (r#"    ~  "#, Token::Mult),
                (r#"      ~"#, Token::Div),
            ],
        )
    }

    #[test]
    fn algorithmic2() {
        test(
            r#"+-*/"#,
            vec![
                (r#"~   "#, Token::Plus),
                (r#" ~  "#, Token::Minus),
                (r#"  ~ "#, Token::Mult),
                (r#"   ~"#, Token::Div),
            ],
        )
    }

    #[test]
    fn comparison() {
        test(
            r#"== != < <= > >="#,
            vec![
                (r#"~~             "#, Token::Eq),
                (r#"   ~~          "#, Token::Neq),
                (r#"      ~        "#, Token::Lt),
                (r#"        ~~     "#, Token::Le),
                (r#"           ~   "#, Token::Gt),
                (r#"             ~~"#, Token::Ge),
            ],
        )
    }

    #[test]
    fn comparison1() {
        test(
            r#"==!=<<=>>="#,
            vec![
                (r#"~~        "#, Token::Eq),
                (r#"  ~~      "#, Token::Neq),
                (r#"    ~     "#, Token::Lt),
                (r#"     ~~   "#, Token::Le),
                (r#"       ~  "#, Token::Gt),
                (r#"        ~~"#, Token::Ge),
            ],
        )
    }

    #[test]
    fn comparison_mix_assign() {
        test(
            r#"===!=<<=>>="#,
            vec![
                (r#"~~         "#, Token::Eq),
                (r#"  ~        "#, Token::Assign),
                (r#"   ~~      "#, Token::Neq),
                (r#"     ~     "#, Token::Lt),
                (r#"      ~~   "#, Token::Le),
                (r#"        ~  "#, Token::Gt),
                (r#"         ~~"#, Token::Ge),
            ],
        )
    }

    #[test]
    fn logical() {
        test(
            r#"! && ||"#,
            vec![
                (r#"~      "#, Token::Not),
                (r#"  ~~   "#, Token::And),
                (r#"     ~~"#, Token::Or),
            ],
        )
    }

    #[test]
    fn brackets_comma() {
        test(
            r#"{[(,)]}"#,
            vec![
                (r#"~      "#, Token::LBrace),
                (r#" ~     "#, Token::LBracket),
                (r#"  ~    "#, Token::LParen),
                (r#"   ~   "#, Token::Comma),
                (r#"    ~  "#, Token::RParen),
                (r#"     ~ "#, Token::RBracket),
                (r#"      ~"#, Token::RBrace),
            ],
        )
    }

    #[test]
    fn match_word1() {
        let text = r#"&[u8]"#;

        let mut tokenizer = Tokenizer::new(text);
        let res = tokenizer.match_word("&[u8]");

        assert_eq!(res, Some(4));
        assert_eq!(tokenizer.next(), Some(Ok((4, Token::RBracket, 5))));
    }

    #[test]
    fn match_word2() {
        let text = r#"&[u8"#;

        let mut tokenizer = Tokenizer::new(text);
        let res = tokenizer.match_word("&[u8]");

        assert_eq!(res, None);
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn match_word3() {
        let text = r#"+ &[u8]+"#;

        let mut tokenizer = Tokenizer::new(text);
        assert_eq!(tokenizer.next_token(), Some(Ok((0, Token::Plus, 1))));
        assert_eq!(tokenizer.peek(), Some((1, ' ')));
        assert_eq!(tokenizer.consume_and_peek(), Some((2, '&')));

        let res = tokenizer.match_word("&[u8]");
        assert_eq!(res, Some(6));

        assert_eq!(tokenizer.consume_and_peek(), Some((7, '+')));
        assert_eq!(tokenizer.next(), Some(Ok((7, Token::Plus, 8))));
    }

    #[test]
    fn builtin_slice_and_and() {
        test(
            r#"&&&[u8] &[u8]"#,
            vec![
                (r#"~~           "#, Token::And),
                (r#"  ~~~~~      "#, Token::BuiltinType("&[u8]")),
                (r#"        ~~~~~"#, Token::BuiltinType("&[u8]")),
            ],
        )
    }

    #[test]
    fn search_for_end_of_code_segment1() {
        let text = r#"%%Option%%"#;

        let mut tokenizer = Tokenizer::new(text);
        tokenizer.consume_and_peek();
        tokenizer.consume_and_peek();

        let res = tokenizer.search_for_end_of_code_segment();
        assert_eq!(res, Some(9));
    }

    #[test]
    fn search_for_end_of_code_segment2() {
        let text = r#"%%Option%"#;

        let mut tokenizer = Tokenizer::new(text);
        tokenizer.consume_and_peek();
        tokenizer.consume_and_peek();

        let res = tokenizer.search_for_end_of_code_segment();
        assert_eq!(res, None);
    }

    #[test]
    fn code_segment_slice() {
        test(
            r#"&[u8], %%&[u8]%%"#,
            vec![
                ("~~~~~           ", Token::BuiltinType("&[u8]")),
                ("     ~          ", Token::Comma),
                ("       ~~~~~~~~~", Token::Code("%%&[u8]%%")),
            ],
        );
    }

    #[test]
    fn key_word1() {
        test(
            r#"packet message iter_group"#,
            vec![
                ("~~~~~~                   ", Token::Packet),
                ("       ~~~~~~~           ", Token::Message),
                ("               ~~~~~~~~~~", Token::IterGroup),
            ],
        );
    }

    #[test]
    fn key_word2() {
        test(
            r#"Field VarField item_size"#,
            vec![
                ("~~~~~                   ", Token::Field),
                ("      ~~~~~~~~          ", Token::VarField),
                ("               ~~~~~~~~~", Token::ItemSize),
            ],
        );
    }

    #[test]
    fn key_word_builtin_type_builtin_value() {
        test(
            r#"cond true u8 u32 bool"#,
            vec![
                ("~~~~                 ", Token::Cond),
                ("     ~~~~            ", Token::BooleanValue("true")),
                ("          ~~         ", Token::BuiltinType("u8")),
                ("             ~~~     ", Token::BuiltinType("u32")),
                ("                 ~~~~", Token::BuiltinType("bool")),
            ],
        );
    }

    #[test]
    fn num_true_false() {
        test(
            r#"5443 4543 00 099 true"#,
            vec![
                ("~~~~                 ", Token::Num("5443")),
                ("     ~~~~            ", Token::Num("4543")),
                ("          ~~         ", Token::Num("00")),
                ("             ~~~     ", Token::Num("099")),
                ("                 ~~~~", Token::BooleanValue("true")),
            ],
        );
    }

    #[test]
    fn search_doc_line1() {
        let doc_line = r#"///"#;

        let mut t = Tokenizer::new(doc_line);
        let res = t.search_for_doc_line();

        assert_eq!(res, Some(2));
    }

    #[test]
    fn search_doc_line2() {
        let doc_line = r#"///abc"#;

        let mut t = Tokenizer::new(doc_line);
        let res = t.search_for_doc_line();

        assert_eq!(res, Some(5));
    }

    #[test]
    fn search_doc_line3() {
        let doc_line = r#"///abc
"#;

        let mut t = Tokenizer::new(doc_line);
        let res = t.search_for_doc_line();

        assert_eq!(res, Some(6));
        assert_eq!(t.consume_and_peek(), None);
    }

    #[test]
    fn doc1() {
        let doc_lines = r#"bbb///abc
a/// wtf???
"#;

        let mut t = Tokenizer::new(doc_lines);

        assert_eq!(t.next_token(), Some(Ok((0, Token::Ident("bbb"), 3))));
        assert_eq!(t.next_token(), Some(Ok((3, Token::Doc("///abc\n"), 10))));
        assert_eq!(t.next_token(), Some(Ok((10, Token::Ident("a"), 11))));
        assert_eq!(
            t.next_token(),
            Some(Ok((11, Token::Doc("/// wtf???\n"), 22)))
        );
    }

    #[test]
    fn doc2() {
        let doc_lines = r#"///a
///b
//
wtf"#;

        let mut t = Tokenizer::new(doc_lines);

        assert_eq!(
            t.next_token(),
            Some(Ok((0, Token::Doc("///a\n///b\n"), 10)))
        );
        assert_eq!(t.next_token(), Some(Ok((13, Token::Ident("wtf"), 16))));
        assert_eq!(t.next_token(), None);
    }

    #[allow(dead_code)]
    #[test]
    fn test_whole_tokenizer() {
        let s = r#"
/// Start
// comment
/// Next
packet Udp {
    // a comment is inserted here
    header = [
        src_port = Field {bit = 16},
        dst_port = Field {bit = 16},
        length = Field {
            bit = 16,
            default = 8,
            gen = false,
        },
        // another comment
        /// The checksum field of the UDP packet
        /// It can be computed with ease
        checksum = Field {bit = 16},
    ],
    with_payload = true,
    packet_len = PacketLen {
        expr = length,
        min = 8,
        max = 65535,
    }
}"#;

        let mut t = Tokenizer::new(s);

        while let Some(Ok(res)) = t.next_token() {
            println!("{:?}", res);
        }

        assert_eq!(t.next_token(), None);
    }
}
