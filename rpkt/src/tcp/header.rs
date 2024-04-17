use byteorder::{ByteOrder, NetworkEndian};

header_field_range_accessors! {
    (src_port, src_port_mut, 0..2),
    (dst_port, dst_port_mut, 2..4),
    (seq_num, seq_num_mut, 4..8),
    (ack_num, ack_num_mut, 8..12),
    (flags, flags_mut, 12..14),
    (win_size, win_size_mut, 14..16),
    (checksum, checksum_mut, 16..18),
    (urgent, urgent_mut, 18..20),
}

const FLG_FIN: u16 = 0x001;
const FLG_SYN: u16 = 0x001 << 1;
const FLG_RST: u16 = 0x001 << 2;
const FLG_PSH: u16 = 0x001 << 3;
const FLG_ACK: u16 = 0x001 << 4;
const FLG_URG: u16 = 0x001 << 5;
const FLG_ECE: u16 = 0x001 << 6;
const FLG_CWR: u16 = 0x001 << 7;
const FLG_NS: u16 = 0x001 << 8;

/// Length of the TCP header without options
pub const TCP_HEADER_LEN: usize = 20;

/// Maximum length of the TCP header with options
pub const TCP_HEADER_LEN_MAX: usize = 60;

pub const TCP_HEADER_TEMPLATE: TcpHeader<[u8; TCP_HEADER_LEN]> = TcpHeader {
    buf: [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00,
    ],
};

///
/// protocol Tcp {
///   src_port: Bit(16), Mult(1) | Start(0), End(None) | Repr(u16), Arg(u16) | Default(0),
///   dst_port: Bit(16), Mult(1) | Start(0), End(None) | Repr(u16), Arg(u16) | Default(0),
///   seq:  Bit(32), Mult(1) | Start(0), End(None) | Repr(u32), Arg(u32) | Default(0),
///   ack:  Bit(32), Mult(1) | Start(0), End(None) | Repr(u32), Arg(u32) | Default(0),
///   header_len:  Bit(4), Mult(4) | Start(20), End(Some(60)) | Repr(u8), Arg(u8) | Default(20),
///   reserved: Bit(4), Mult(1) | Start(0), End(Some(0)) | Repr(u8), Arg(u8) | Default(0),
///   cwr: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   ece: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   urg: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   ack: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   psh: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   rst: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   syn: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   fin: Bit(1), Mult(1) | Start(0), End(Some(1)) | Repr(u8), Arg(bool) | Default(false),
///   window_size: Bit(16), Mult(1) | Start(0), End(None) | Repr(u16), Arg(u16) | Default(0),
///   checksum: Bit(16), Mult(1) | Start(0), End(None) | Repr(u16), Arg(u16) | Default(0),
///   urgent_pointer: Bit(16), Mult(1) | Start(0), End(None) | Repr(u16), Arg(u16) | Default(0),
///   option: Variable | Start(0), End(Some(header_len-20))
/// }
#[derive(Clone, Copy, Debug)]
pub struct TcpHeader<T> {
    buf: T,
}

impl<T: AsRef<[u8]>> TcpHeader<T> {
    #[inline]
    pub fn new(buf: T) -> Result<Self, T> {
        if buf.as_ref().len() >= TCP_HEADER_LEN {
            Ok(Self { buf })
        } else {
            Err(buf)
        }
    }

    #[inline]
    pub fn new_unchecked(buf: T) -> Self {
        Self { buf }
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.buf.as_ref()[0..TCP_HEADER_LEN]
    }

    #[inline]
    pub fn to_owned(&self) -> TcpHeader<[u8; TCP_HEADER_LEN]> {
        let mut buf = [0; TCP_HEADER_LEN];
        buf.copy_from_slice(self.as_bytes());
        TcpHeader { buf }
    }

    #[inline]
    pub fn header_len(&self) -> u8 {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        ((raw & 0xf000) >> 10) as u8
    }

    #[inline]
    pub fn src_port(&self) -> u16 {
        let data = src_port(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn dst_port(&self) -> u16 {
        let data = dst_port(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn seq_number(&self) -> u32 {
        let data = seq_num(self.buf.as_ref());
        NetworkEndian::read_u32(data)
    }

    #[inline]
    pub fn ack_number(&self) -> u32 {
        let data = ack_num(self.buf.as_ref());
        NetworkEndian::read_u32(data)
    }

    #[inline]
    pub fn fin(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_FIN != 0
    }

    #[inline]
    pub fn syn(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_SYN != 0
    }

    #[inline]
    pub fn rst(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_RST != 0
    }

    #[inline]
    pub fn psh(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_PSH != 0
    }

    #[inline]
    pub fn ack(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_ACK != 0
    }

    #[inline]
    pub fn urg(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_URG != 0
    }

    #[inline]
    pub fn ece(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_ECE != 0
    }

    #[inline]
    pub fn cwr(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_CWR != 0
    }

    #[inline]
    pub fn ns(&self) -> bool {
        let data = flags(self.buf.as_ref());
        let raw = NetworkEndian::read_u16(data);
        raw & FLG_NS != 0
    }

    #[inline]
    pub fn check_reserved(&self) -> bool {
        let data = flags(self.buf.as_ref());
        (NetworkEndian::read_u16(data) & 0x0e00) >> 9 == 0
    }

    #[inline]
    pub fn window_size(&self) -> u16 {
        let data = win_size(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn checksum(&self) -> u16 {
        let data = checksum(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn urgent_ptr(&self) -> u16 {
        let data = urgent(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }
}

impl<T: AsMut<[u8]>> TcpHeader<T> {
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!(value >= 20 && value <= 60 && value & 0x03 == 0);
        let data = flags_mut(self.buf.as_mut());
        let raw = NetworkEndian::read_u16(data);
        let raw = (raw & !0xf000) | ((value as u16) << 10);
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_src_port(&mut self, value: u16) {
        let data = src_port_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }

    #[inline]
    pub fn set_dst_port(&mut self, value: u16) {
        let data = dst_port_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }

    #[inline]
    pub fn set_seq_number(&mut self, value: u32) {
        let data = seq_num_mut(self.buf.as_mut());
        NetworkEndian::write_u32(data, value)
    }

    #[inline]
    pub fn set_ack_number(&mut self, value: u32) {
        let data = ack_num_mut(self.buf.as_mut());
        NetworkEndian::write_u32(data, value)
    }

    #[inline]
    pub fn clear_flags(&mut self) {
        let data = flags_mut(self.buf.as_mut());
        let raw = NetworkEndian::read_u16(data);
        let raw = raw & !0x0fff;
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_fin(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_FIN
        } else {
            NetworkEndian::read_u16(data) & !FLG_FIN
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_syn(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_SYN
        } else {
            NetworkEndian::read_u16(data) & !FLG_SYN
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_rst(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_RST
        } else {
            NetworkEndian::read_u16(data) & !FLG_RST
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_psh(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_PSH
        } else {
            NetworkEndian::read_u16(data) & !FLG_PSH
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_ack(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_ACK
        } else {
            NetworkEndian::read_u16(data) & !FLG_ACK
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_urg(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_URG
        } else {
            NetworkEndian::read_u16(data) & !FLG_URG
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_ece(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_ECE
        } else {
            NetworkEndian::read_u16(data) & !FLG_ECE
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_cwr(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_CWR
        } else {
            NetworkEndian::read_u16(data) & !FLG_CWR
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_ns(&mut self, value: bool) {
        let data = flags_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | FLG_NS
        } else {
            NetworkEndian::read_u16(data) & !FLG_NS
        };
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn adjust_reserved(&mut self) {
        let data = flags_mut(self.buf.as_mut());
        let raw = NetworkEndian::read_u16(data) & 0xf1ff;
        NetworkEndian::write_u16(data, raw)
    }

    #[inline]
    pub fn set_window_size(&mut self, value: u16) {
        let data = win_size_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }

    #[inline]
    pub fn set_checksum(&mut self, value: u16) {
        let data = checksum_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }

    #[inline]
    pub fn set_urgent_ptr(&mut self, value: u16) {
        let data = urgent_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }
}
