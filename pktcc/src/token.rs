quick_error! {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Error {
        InvalidToken(pos: usize) {
            display("invalid token at {}", pos)
        }
        UnclosedCodeSegment(pos: usize) {
            display("unclosed code segment at {}", pos)
        }
    }
}

pub type Spanned<T> = (usize, T, usize);

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

    // peek the stream head
    fn peek(&self) -> Option<(usize, char)> {
        self.head
    }

    // consume the current stream head,
    // and return the next stream head
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

    // match the stream agaist the input word, until the
    // last character of the word is found
    // return the byteoffset of the last character of a word
    // this method does not consume the last character from the input stream
    fn match_word(&mut self, word: &str) -> Option<usize> {
        // make sure that the input word is not empty
        assert!(word.len() > 0);

        let mut word_chars = word.chars().peekable();
        let mut match_last = false;
        let f = |c: char| -> bool {
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
        };
        self.take_while(f);

        if match_last {
            self.peek().map(|(offset, _)| offset)
        } else {
            None
        }
    }

    // search for the last character of a code segment
    // it will make the stream stays on the closing '%' symbol
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

    fn next_token(&mut self) -> Option<Result<Spanned<Token<'input>>, Error>> {
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
                Some((idx, '/')) => {
                    self.consume_and_peek();
                    return Some(Ok((idx, Token::Div, idx + 1)));
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
                            return Some(Err(Error::InvalidToken(idx)));
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
                            .unwrap_or(Some(Err(Error::UnclosedCodeSegment(idx))));
                    }
                    _ => {
                        return Some(Err(Error::InvalidToken(idx)));
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
                Some((idx, c)) => {
                    println!("{}, {}", idx, c);
                    return Some(Err(Error::InvalidToken(idx)));
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
    type Item = Result<Spanned<Token<'input>>, Error>;

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
    fn test_goback() {
        let s = "abcdefghijklmnopqrstuvwxyz";
        let mut i = s.char_indices();
        for _ in 0..10 {
            i.next();
        }

        println!("{:?}", i.next());

        let s_new = &s[10..];

        let mut i_new = s_new.char_indices();
        println!("{:?}", i_new.next());
    }
}
