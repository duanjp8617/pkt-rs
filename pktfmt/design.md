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

# The header container:

1. transform buffer to header container: 
* `parse`: with strict length checking
* `parse_unchecked`: without length checking

2. transform header container to buffer:
* `release`: return the buffer without any checks

3. access the fixed header as a byte slice:
* `as_bytes`: return an imutable byte slice, whose length equals the fixed header length
* `as_bytes_mut`: the mutable version of `as_bytes`

4. Header field getters and setters.

5. Length field getters and setters.

# The packet container:

1. transform buffer to packet container: 
* `parse`: with strict length checking
* `parse_unchecked`: without length checking
* `prepend_header`: push a header to the start of the current payload

2. transform header container to buffer:
* `release`: return the buffer without any checks
* `payload`: return a buffer with header beinng removed and only payload contained.

3. access the fixed header as a byte slice:
* `header`: return an imutable byte slice, whose length equals the fixed header length
* the corresponding `header_mut` is not provided, because directly modifying the imutable byte slice of the header may corrupt the packet data, resulting in invalid packet format.

4. Header field getters and setters.

5. Length field getters and setters.

6. Return a byte slice covering the variable option bytes, if the packet has such structure. 

7. Return a imutable reference to the underlying buffer, as the information of the underlying buffer may be needed at sometime. 

# The generated methods.

The generated methods should include the following types.

1. Transformation between the corresponding container type and the buffer type. 
* buffer type -> container type
    ** `parse`: convert buffer to container with strict length checking
    ** `parse_unchecked`: convert buffer to container without any checks
    ** `prepend_header`: convert buffer to container by prepending an existing header type, this only appears for packet container
    ** `build_message`: convert buffer to container by copying a default header type to the start of the container, this only appears for message container


2. etc
* This is generally expressed through the `parse`, `parse_unchecked`, `release` and `payload` methods.
* `parse` tansforms the buffer type into the container type and applies thorough length format checking.
* `parse_unchecked` transforms the buffer type into the container type without applying any length format checking.
* `release` method takes the buffer out of the container.
* `payload` method removes the header (including the variable part), and return only the buffer containing the payload.