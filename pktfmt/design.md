# Design of pktfmt script language

```json
packet {
    header = [
        (field_name = Field {
            bit = num,
            (, repr = u8|u16|u32|u64|&[u8])?
            (, arg = u8|u16|u32|u64|&[u8]|bool|user-defined-rust-type)?
            (, default = num|&[u8])?
            (, gen = true|false)?
        },)+
    ],
    length = [
        (header_len = expr,)?
        (payload_len = expr,)?
        (packet_len = expr,)?
    ],
}
```

```json
message {
    header = [
        (field_name = Field {
            bit = num,
            (, repr = u8|u16|u32|u64|&[u8])?
            (, arg = u8|u16|u32|u64|&[u8]|bool|user-defined-rust-type)?
            (, default = num|&[u8])?
            (, gen = true|false)?
        },)+
    ],
    length = [
        (header_len = expr,)?
    ],
    cond = cond_expr
}
```