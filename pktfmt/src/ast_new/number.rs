use std::str::FromStr;

use super::Error;

/// The maximum size of a contiguous memory region for storing the packet.
pub const MAX_MTU_IN_BYTES: u64 = 2_u64.pow(20) - 1;

// in our case, if the packet defines header length and payload length,
// then the maximum MTU size calculated can be MAX_MTU_IN_BYTES * 2
// We need to make sure that MAX_MTU_IN_BYTES * 2 can be safely converted
// to the usize type
const fn _check_mtu_for_usize() -> bool {
    if std::mem::size_of::<usize>() > std::mem::size_of::<u64>() {
        ((MAX_MTU_IN_BYTES * 2) as usize) < usize::MAX
    } else {
        //
        MAX_MTU_IN_BYTES * 2 < usize::MAX as u64
    }
}

// const block is stabilized in 1.79. Now we have complete mechanisms for
// triggering compile-time errors.
const _: () = assert!(_check_mtu_for_usize());

// parse the byte token from a byte array to u8
pub fn parse_to_byte_val(token_s: &str) -> Result<u8, Error> {
    // the parse always succeeds, because token_s is a parsed number token
    let num = u64::from_str(token_s).unwrap();

    if num > 255 {
        return_err_1!(Error::num_error(format!("invalid byte value {}", num)))
    } else {
        Ok(num as u8)
    }
}
