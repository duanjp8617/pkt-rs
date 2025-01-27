use super::Error;

/// The maximum size of a contiguous memory region for storing the packet.
pub const MAX_MTU_IN_BYTES: u64 = (1 << 22) - 1;

// in our case, if the packet defines header length and payload length,
// then the maximum MTU size calculated can be MAX_MTU_IN_BYTES * 2
// We need to make sure that MAX_MTU_IN_BYTES * 2 can be safely converted
// to the usize type
const fn _check_mtu_for_usize() -> bool {
    MAX_MTU_IN_BYTES * 2 < usize::MAX as u64
}

// we assume to only work on systems whose bit width is smaller than 64.
const fn _check_usize() -> bool {
    std::mem::size_of::<usize>() <= std::mem::size_of::<u64>()
}

// const block is stabilized in 1.79. Now we have complete mechanisms for
// triggering compile-time errors.
const _: () = assert!(_check_usize() && _check_mtu_for_usize());

// parse the byte token from a byte array to u8
pub(crate) fn parse_to_byte_val(num: u64) -> Result<u8, Error> {
    if num > 255 {
        return_err!(Error::num_error(1, format!("invalid byte value {}", num)))
    } else {
        Ok(num as u8)
    }
}
