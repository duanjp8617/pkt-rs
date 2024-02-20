use crate::*;

#[test]
fn field_e1() {
    let fname_ut = "field_e1.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e1.pktfmt 2:1-5:1
    ~~~~~~~
2 | Field {
3 |     bit = 8, 
4 |     repr = u16,
5 | }
    ^
invalid Field definition: invalid repr
If repr is defined, it must satisfy one of the following conditions:
1. The defined repr is the same as the repr inferred from the bit.
2. The defined repr is a byte slice, and bit % 8 == 0
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e2() {
    let fname_ut = "field_e2.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e2.pktfmt 2:1-5:1
    ~~~~~~~
2 | Field {
3 |     bit = 5, 
4 |     repr = &[u8],
5 | }
    ^
invalid Field definition: invalid repr
If repr is defined, it must satisfy one of the following conditions:
1. The defined repr is the same as the repr inferred from the bit.
2. The defined repr is a byte slice, and bit % 8 == 0
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e3() {
    let fname_ut = "field_e3.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e3.pktfmt 2:1-6:1
    ~~~~~~~
2 | Field {
3 |     bit = 1, 
4 |     repr = u8,
5 |     arg = u16,
6 | }
    ^
invalid Field definition: invalid arg
If arg is defined, it must satisfy one of the following conditions:
1. The defined arg is the same as the current repr (either defined or inferred).
2. The defined arg is bool, and the bit size is 1.
3. The defined arg is a standard Rust type wrapped by %% as code segment.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e4() {
    let fname_ut = "field_e4.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e4.pktfmt 2:1-6:1
    ~~~~~~~
2 | Field {
3 |     bit = 2, 
4 |     repr = u8,
5 |     arg = bool,
6 | }
    ^
invalid Field definition: invalid arg
If arg is defined, it must satisfy one of the following conditions:
1. The defined arg is the same as the current repr (either defined or inferred).
2. The defined arg is bool, and the bit size is 1.
3. The defined arg is a standard Rust type wrapped by %% as code segment.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e5() {
    let fname_ut = "field_e5.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e5.pktfmt 2:1-6:1
    ~~~~~~~
2 | Field {
3 |     bit = 2, 
4 |     repr = u8,
5 |     arg = %%Ipv4Addr%%,
6 | }
    ^
invalid Field definition: missing default
The arg (either defined or inferred) is a Rust code segment, and the default must be 
provided as a Rust code segment.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e6() {
    let fname_ut = "field_e6.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e6.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 1, 
4 |     repr = u8,
5 |     arg = bool,
6 |     default = 0,
7 | }
    ^
invalid Field definition: invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e7() {
    let fname_ut = "field_e7.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e7.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 8, 
4 |     repr = &[u8],
5 |     arg = &[u8],
6 |     default = 0,
7 | }
    ^
invalid Field definition: invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e8() {
    let fname_ut = "field_e8.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e8.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 5, 
4 |     repr = u8,
5 |     arg = u8,
6 |     default = true,
7 | }
    ^
invalid Field definition: invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e9() {
    let fname_ut = "field_e9.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e9.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 1, 
4 |     repr = u8,
5 |     arg = %%Ipv4Addr%%,
6 |     default = 0,
7 | }
    ^
invalid Field definition: invalid default
If arg is a code segment, then the default must be a code segment as well.
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}

#[test]
fn field_e10() {
    let fname_ut = "field_e10.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e10.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 0, 
4 |     repr = u8,
5 |     arg = %%Ipv4Addr%%,
6 |     default = 0,
7 | }
    ^
invalid Field definition: invalid bit size
the following expression should not hold: 
bit == 0 || (bit > 64 && bit % 8 != 0)
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}
