use pktfmt::*;

#[macro_use]
mod common;

#[test]
fn field_invalid_repr1() {
    let err_file = "field_invalid_repr1.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_repr1.pktfmt 2:1-5:1
    ~~~~~~~
2 | Field {
3 |     bit = 8, 
4 |     repr = u16,
5 | }
    ^
note: invalid Field definition: invalid repr
If repr is defined, it must satisfy the following conditions:
1. The defined repr is the same as the repr inferred from the bit.
2. The defined repr is a byte slice, and bit % 8 == 0
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_repr2() {
    let err_file = "field_invalid_repr2.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_repr2.pktfmt 2:1-5:1
    ~~~~~~~
2 | Field {
3 |     bit = 5, 
4 |     repr = &[u8],
5 | }
    ^
note: invalid Field definition: invalid repr
If repr is defined, it must satisfy the following conditions:
1. The defined repr is the same as the repr inferred from the bit.
2. The defined repr is a byte slice, and bit % 8 == 0
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_arg1() {
    let err_file = "field_invalid_arg1.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_arg1.pktfmt 2:1-6:1
    ~~~~~~~
2 | Field {
3 |     bit = 1, 
4 |     repr = u8,
5 |     arg = u16,
6 | }
    ^
note: invalid Field definition: invalid arg
If arg is defined, it must satisfy one of the following conditions:
1. The defined arg is the same as the current repr (either defined or inferred).
2. The defined arg is bool, and the bit size is 1.
3. The defined arg is a standard Rust type wrapped by %% as code segment.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_arg2() {
    let err_file = "field_invalid_arg2.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_arg2.pktfmt 2:1-6:1
    ~~~~~~~
2 | Field {
3 |     bit = 2, 
4 |     repr = u8,
5 |     arg = bool,
6 | }
    ^
note: invalid Field definition: invalid arg
If arg is defined, it must satisfy one of the following conditions:
1. The defined arg is the same as the current repr (either defined or inferred).
2. The defined arg is bool, and the bit size is 1.
3. The defined arg is a standard Rust type wrapped by %% as code segment.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_arg3() {
    let err_file = "field_invalid_arg3.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_arg3.pktfmt 2:1-6:1
    ~~~~~~~
2 | Field {
3 |     bit = 2, 
4 |     repr = u8,
5 |     arg = %%Ipv4Addr%%,
6 | }
    ^
note: invalid Field definition: missing default
The arg (either defined or inferred) is a Rust code segment, and the default must be 
provided as a Rust code segment.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_default1() {
    let err_file = "field_invalid_default1.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_default1.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 1, 
4 |     repr = u8,
5 |     arg = bool,
6 |     default = 0,
7 | }
    ^
note: invalid Field definition: invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_default2() {
    let err_file = "field_invalid_default2.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_default2.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 8, 
4 |     repr = &[u8],
5 |     arg = &[u8],
6 |     default = 0,
7 | }
    ^
note: invalid Field definition: invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_default3() {
    let err_file = "field_invalid_default3.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_default3.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 5, 
4 |     repr = u8,
5 |     arg = u8,
6 |     default = true,
7 | }
    ^
note: invalid Field definition: invalid default 
If arg is a builtin type and default is defined, the default must satisfy one of the 
following conditions: 
1. The arg is bool and the default is a boolean value.
2. The arg is a byte slice and the default is a code segment
3. The arg is u8/u16/u32/u64 and the default is a number.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_default4() {
    let err_file = "field_invalid_default4.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_default4.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 1, 
4 |     repr = u8,
5 |     arg = %%Ipv4Addr%%,
6 |     default = 0,
7 | }
    ^
note: invalid Field definition: invalid default
If arg is a code segment, then the default must be a code segment as well.
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}

#[test]
fn field_invalid_bit() {
    let err_file = "field_invalid_bit.pktfmt";
    
    let err_reason = 
r#"error: invalid Field definition
at /home/djp/pkt-rs/pktfmt/tests/error_fmts/field_invalid_bit.pktfmt 2:1-7:1
    ~~~~~~~
2 | Field {
3 |     bit = 0, 
4 |     repr = u8,
5 |     arg = %%Ipv4Addr%%,
6 |     default = 0,
7 | }
    ^
note: invalid Field definition: invalid bit size
the following expression should not hold: 
bit == 0 || (bit > 64 && bit % 8 != 0)
"#;

    test_parse_error!(err_file, err_reason, pktfmt::parser::FieldParser);
}