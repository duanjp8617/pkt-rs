quick_error! {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Error {
        InvalidToken(pos: usize) {
            display("invalid token at {}", pos)
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
    Len,
    ItemSize,

    // HeaderField keyworld
    Expr,
    Min,
    Max,

    // Identifiers
    NonCamelCaseIdent(&'input str),
    CamelCaseIdent(&'input str),

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

    // qouted string "RsType::new(1)"
    QoutedString(&'input str),

    // Prefix string
    // xxxx %%
    PrefixString(&'input str),

    // Postfix string
    // %% xxxx
    PostfixString(&'input str),

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
    ("len", Token::Len),
    ("item_size", Token::ItemSize),
    ("expr", Token::Expr),
    ("min", Token::Min),
    ("max", Token::Max),
];

const BUILTINS: &[&str] = &["u8", "u16", "u32", "u64", "bool", "true", "false"];

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

    // consume a word from the stream, return true if succeed
    fn consume_word(&mut self, word: &str) -> bool {
        let mut word_chars = word.chars().peekable();
        let f = |c: char| -> bool {
            match word_chars.peek() {
                Some(expected_c) => {
                    if *expected_c == c {
                        word_chars.next();
                        true
                    } else {
                        false
                    }
                }
                None => false,
            }
        };

        self.take_while(f);
        word_chars.peek().is_none()
    }

    fn next_token(&mut self) -> Option<Result<Spanned<Token<'input>>, Error>> {
        loop {
            match self.head {
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
                    _ => {
                        if self.consume_word("[u8]") {
                            return Some(Ok((
                                idx,
                                Token::BuiltinType(&self.text[idx..idx + 5]),
                                idx + 5,
                            )));
                        } else {
                            return Some(Err(Error::InvalidToken(idx)));
                        }
                    }
                },
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
    fn test_consume_word1() {
        let text = r#"&[u8]"#;

        let mut tokenizer = Tokenizer::new(text);
        let res = tokenizer.consume_word("&[u8]");

        assert_eq!(res, true);
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_consume_word2() {
        let text = r#"&[u8"#;

        let mut tokenizer = Tokenizer::new(text);
        let res = tokenizer.consume_word("&[u8]");

        assert_eq!(res, false);
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_consume_word3() {
        let text = r#"&[u8] +"#;

        let mut tokenizer = Tokenizer::new(text);
        let res = tokenizer.consume_word("&[u8]");

        assert_eq!(res, true);
        assert_eq!(tokenizer.peek(), Some((5, ' ')));
        assert_eq!(tokenizer.next(), Some(Ok((6, Token::Plus, 7))));
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
}
