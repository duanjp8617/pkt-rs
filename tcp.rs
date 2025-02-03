use std::io;

/// A constant that defines the fixed byte length of the Tcp protocol header.
pub const TCP_HEADER_LEN: usize = 20;
/// A fixed Tcp header.
pub const TCP_HEADER_TEMPLATE: TcpHeader<[u8; 20]> = TcpHeader {
    buf: [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00,
    ],
};
#[derive(Debug, Clone, Copy)]
pub struct TcpHeader<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> TcpHeader<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 20 {
            return Err(buf);
        }
        let container = Self { buf };
        Ok(container)
    }
    #[inline]
    pub fn header_slice(&self) -> &[u8] {
        &self.buf.as_ref()[0..20]
    }
    #[inline]
    pub fn src_port(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.as_ref()[0..2])
    }
    #[inline]
    pub fn dst_port(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.as_ref()[2..4])
    }
    #[inline]
    pub fn seq_num(&self) -> u32 {
        NetworkEndian::read_u32(&self.buf.as_ref()[4..8])
    }
    #[inline]
    pub fn ack_num(&self) -> u32 {
        NetworkEndian::read_u32(&self.buf.as_ref()[8..12])
    }
    #[inline]
    pub fn reserved(&self) -> u8 {
        self.buf.as_ref()[12] & 0xf
    }
    #[inline]
    pub fn cwr(&self) -> bool {
        self.buf.as_ref()[13] & 0x80 != 0
    }
    #[inline]
    pub fn ece(&self) -> bool {
        self.buf.as_ref()[13] & 0x40 != 0
    }
    #[inline]
    pub fn urg(&self) -> bool {
        self.buf.as_ref()[13] & 0x20 != 0
    }
    #[inline]
    pub fn ack(&self) -> bool {
        self.buf.as_ref()[13] & 0x10 != 0
    }
    #[inline]
    pub fn psh(&self) -> bool {
        self.buf.as_ref()[13] & 0x8 != 0
    }
    #[inline]
    pub fn rst(&self) -> bool {
        self.buf.as_ref()[13] & 0x4 != 0
    }
    #[inline]
    pub fn syn(&self) -> bool {
        self.buf.as_ref()[13] & 0x2 != 0
    }
    #[inline]
    pub fn fin(&self) -> bool {
        self.buf.as_ref()[13] & 0x1 != 0
    }
    #[inline]
    pub fn window_size(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.as_ref()[14..16])
    }
    #[inline]
    pub fn checksum(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.as_ref()[16..18])
    }
    #[inline]
    pub fn urgent_pointer(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.as_ref()[18..20])
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.as_ref()[12] >> 4) * 4
    }
}
impl<T: AsMut<[u8]>> TcpHeader<T> {
    #[inline]
    pub fn header_slice_mut(&mut self) -> &mut [u8] {
        &mut self.buf.as_mut()[0..20]
    }
    #[inline]
    pub fn set_src_port(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.as_mut()[0..2], value);
    }
    #[inline]
    pub fn set_dst_port(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.as_mut()[2..4], value);
    }
    #[inline]
    pub fn set_seq_num(&mut self, value: u32) {
        NetworkEndian::write_u32(&mut self.buf.as_mut()[4..8], value);
    }
    #[inline]
    pub fn set_ack_num(&mut self, value: u32) {
        NetworkEndian::write_u32(&mut self.buf.as_mut()[8..12], value);
    }
    #[inline]
    pub fn set_reserved(&mut self, value: u8) {
        assert!(value <= 0xf);
        self.buf.as_mut()[12] = (self.buf.as_mut()[12] & 0xf0) | value;
    }
    #[inline]
    pub fn set_cwr(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x80
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0x7f
        }
    }
    #[inline]
    pub fn set_ece(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x40
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xbf
        }
    }
    #[inline]
    pub fn set_urg(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x20
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xdf
        }
    }
    #[inline]
    pub fn set_ack(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x10
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xef
        }
    }
    #[inline]
    pub fn set_psh(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x8
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xf7
        }
    }
    #[inline]
    pub fn set_rst(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x4
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xfb
        }
    }
    #[inline]
    pub fn set_syn(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x2
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xfd
        }
    }
    #[inline]
    pub fn set_fin(&mut self, value: bool) {
        if value {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] | 0x1
        } else {
            self.buf.as_mut()[13] = self.buf.as_mut()[13] & 0xfe
        }
    }
    #[inline]
    pub fn set_window_size(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.as_mut()[14..16], value);
    }
    #[inline]
    pub fn set_checksum(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.as_mut()[16..18], value);
    }
    #[inline]
    pub fn set_urgent_pointer(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.as_mut()[18..20], value);
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value <= 60) && (value % 4 == 0));
        self.buf.as_mut()[12] = (self.buf.as_mut()[12] & 0x0f) | ((value / 4) << 4);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TcpPacket<T> {
    buf: T,
}
impl<T: PktBuf> TcpPacket<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let chunk_len = buf.chunk().len();
        if chunk_len < 20 {
            return Err(buf);
        }
        let container = Self { buf };
        if ((container.header_len() as usize) < 20)
            || ((container.header_len() as usize) > chunk_len)
        {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(self) -> T {
        let header_len = self.header_len() as usize;
        let mut buf = self.buf;
        buf.advance(header_len);
        buf
    }
    #[inline]
    pub fn header_slice(&self) -> &[u8] {
        &self.buf.chunk()[0..20]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.chunk()[20..header_len]
    }
    #[inline]
    pub fn src_port(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.chunk()[0..2])
    }
    #[inline]
    pub fn dst_port(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.chunk()[2..4])
    }
    #[inline]
    pub fn seq_num(&self) -> u32 {
        NetworkEndian::read_u32(&self.buf.chunk()[4..8])
    }
    #[inline]
    pub fn ack_num(&self) -> u32 {
        NetworkEndian::read_u32(&self.buf.chunk()[8..12])
    }
    #[inline]
    pub fn reserved(&self) -> u8 {
        self.buf.chunk()[12] & 0xf
    }
    #[inline]
    pub fn cwr(&self) -> bool {
        self.buf.chunk()[13] & 0x80 != 0
    }
    #[inline]
    pub fn ece(&self) -> bool {
        self.buf.chunk()[13] & 0x40 != 0
    }
    #[inline]
    pub fn urg(&self) -> bool {
        self.buf.chunk()[13] & 0x20 != 0
    }
    #[inline]
    pub fn ack(&self) -> bool {
        self.buf.chunk()[13] & 0x10 != 0
    }
    #[inline]
    pub fn psh(&self) -> bool {
        self.buf.chunk()[13] & 0x8 != 0
    }
    #[inline]
    pub fn rst(&self) -> bool {
        self.buf.chunk()[13] & 0x4 != 0
    }
    #[inline]
    pub fn syn(&self) -> bool {
        self.buf.chunk()[13] & 0x2 != 0
    }
    #[inline]
    pub fn fin(&self) -> bool {
        self.buf.chunk()[13] & 0x1 != 0
    }
    #[inline]
    pub fn window_size(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.chunk()[14..16])
    }
    #[inline]
    pub fn checksum(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.chunk()[16..18])
    }
    #[inline]
    pub fn urgent_pointer(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.chunk()[18..20])
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.chunk()[12] >> 4) * 4
    }
}
impl<T: BufMut> TcpPacket<T> {
    #[inline]
    pub fn prepend_header<HT: AsRef<[u8]>>(mut buf: T, header: &TcpHeader<HT>) -> Self {
        let header_len = header.header_len() as usize;
        assert!((header_len >= 20) && (header_len <= buf.chunk_headroom()));
        buf.move_back(header_len);
        (&mut buf.chunk_mut()[0..20]).copy_from_slice(header.header_slice());
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.chunk_mut()[20..header_len]
    }
    #[inline]
    pub fn set_src_port(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.chunk_mut()[0..2], value);
    }
    #[inline]
    pub fn set_dst_port(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.chunk_mut()[2..4], value);
    }
    #[inline]
    pub fn set_seq_num(&mut self, value: u32) {
        NetworkEndian::write_u32(&mut self.buf.chunk_mut()[4..8], value);
    }
    #[inline]
    pub fn set_ack_num(&mut self, value: u32) {
        NetworkEndian::write_u32(&mut self.buf.chunk_mut()[8..12], value);
    }
    #[inline]
    pub fn set_reserved(&mut self, value: u8) {
        assert!(value <= 0xf);
        self.buf.chunk_mut()[12] = (self.buf.chunk_mut()[12] & 0xf0) | value;
    }
    #[inline]
    pub fn set_cwr(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x80
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0x7f
        }
    }
    #[inline]
    pub fn set_ece(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x40
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xbf
        }
    }
    #[inline]
    pub fn set_urg(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x20
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xdf
        }
    }
    #[inline]
    pub fn set_ack(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x10
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xef
        }
    }
    #[inline]
    pub fn set_psh(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x8
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xf7
        }
    }
    #[inline]
    pub fn set_rst(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x4
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xfb
        }
    }
    #[inline]
    pub fn set_syn(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x2
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xfd
        }
    }
    #[inline]
    pub fn set_fin(&mut self, value: bool) {
        if value {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] | 0x1
        } else {
            self.buf.chunk_mut()[13] = self.buf.chunk_mut()[13] & 0xfe
        }
    }
    #[inline]
    pub fn set_window_size(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.chunk_mut()[14..16], value);
    }
    #[inline]
    pub fn set_checksum(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.chunk_mut()[16..18], value);
    }
    #[inline]
    pub fn set_urgent_pointer(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.chunk_mut()[18..20], value);
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value <= 60) && (value % 4 == 0));
        self.buf.chunk_mut()[12] = (self.buf.chunk_mut()[12] & 0x0f) | ((value / 4) << 4);
    }
}
impl<'a> TcpPacket<Cursor<'a>> {
    #[inline]
    pub fn parse_for_cursor(buf: Cursor<'a>) -> Result<Self, Cursor<'a>> {
        let remaining_len = buf.chunk().len();
        if remaining_len < 20 {
            return Err(buf);
        }
        let container = Self { buf };
        if ((container.header_len() as usize) < 20)
            || ((container.header_len() as usize) > remaining_len)
        {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload_for_cursor(&self) -> Cursor<'_> {
        let header_len = self.header_len() as usize;
        Cursor::new(&self.buf.chunk()[header_len..])
    }
}
impl<'a> TcpPacket<CursorMut<'a>> {
    #[inline]
    pub fn parse_for_cursor_mut(buf: CursorMut<'a>) -> Result<Self, CursorMut<'a>> {
        let remaining_len = buf.chunk().len();
        if remaining_len < 20 {
            return Err(buf);
        }
        let container = Self { buf };
        if ((container.header_len() as usize) < 20)
            || ((container.header_len() as usize) > remaining_len)
        {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload_for_cursor_mut(&mut self) -> CursorMut<'_> {
        let header_len = self.header_len() as usize;
        CursorMut::new(&mut self.buf.chunk_mut()[header_len..])
    }
}

/// A fixed Eol header array.
pub const EOL_HEADER_ARRAY: [u8; 1] = [0x00];
#[derive(Debug, Clone, Copy)]
pub struct EolMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> EolMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 1 {
            return Err(buf);
        }
        let container = Self { buf };
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        &self.buf.as_ref()[1..]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> EolMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        &mut self.buf.as_mut()[1..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 1);
        &mut buf.as_mut()[..1].copy_from_slice(EOL_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 0);
        self.buf.as_mut()[0] = value;
    }
}

/// A fixed Nop header array.
pub const NOP_HEADER_ARRAY: [u8; 1] = [0x01];
#[derive(Debug, Clone, Copy)]
pub struct NopMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> NopMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 1 {
            return Err(buf);
        }
        let container = Self { buf };
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        &self.buf.as_ref()[1..]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> NopMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        &mut self.buf.as_mut()[1..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 1);
        &mut buf.as_mut()[..1].copy_from_slice(NOP_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 1);
        self.buf.as_mut()[0] = value;
    }
}

/// A fixed Mss header array.
pub const MSS_HEADER_ARRAY: [u8; 4] = [0x02, 0x04, 0x00, 0x00];
#[derive(Debug, Clone, Copy)]
pub struct MssMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> MssMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 4 {
            return Err(buf);
        }
        let container = Self { buf };
        if (container.header_len() as usize) != 4 {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        let header_len = self.header_len() as usize;
        &self.buf.as_ref()[header_len..]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.as_ref()[4..header_len]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
    #[inline]
    pub fn mss(&self) -> u16 {
        NetworkEndian::read_u16(&self.buf.as_ref()[2..4])
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.as_ref()[1])
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> MssMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len() as usize;
        &mut self.buf.as_mut()[header_len..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 4);
        &mut buf.as_mut()[..4].copy_from_slice(MSS_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.as_mut()[4..header_len]
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 2);
        self.buf.as_mut()[0] = value;
    }
    #[inline]
    pub fn set_mss(&mut self, value: u16) {
        NetworkEndian::write_u16(&mut self.buf.as_mut()[2..4], value);
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value == 4));
        self.buf.as_mut()[1] = (value);
    }
}

/// A fixed Wsopt header array.
pub const WSOPT_HEADER_ARRAY: [u8; 3] = [0x03, 0x03, 0x00];
#[derive(Debug, Clone, Copy)]
pub struct WsoptMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> WsoptMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 3 {
            return Err(buf);
        }
        let container = Self { buf };
        if (container.header_len() as usize) != 3 {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        let header_len = self.header_len() as usize;
        &self.buf.as_ref()[header_len..]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.as_ref()[3..header_len]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
    #[inline]
    pub fn wsopt(&self) -> u8 {
        self.buf.as_ref()[2]
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.as_ref()[1])
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> WsoptMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len() as usize;
        &mut self.buf.as_mut()[header_len..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 3);
        &mut buf.as_mut()[..3].copy_from_slice(WSOPT_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.as_mut()[3..header_len]
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 3);
        self.buf.as_mut()[0] = value;
    }
    #[inline]
    pub fn set_wsopt(&mut self, value: u8) {
        self.buf.as_mut()[2] = value;
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value == 3));
        self.buf.as_mut()[1] = (value);
    }
}

/// A fixed Sackperm header array.
pub const SACKPERM_HEADER_ARRAY: [u8; 2] = [0x04, 0x02];
#[derive(Debug, Clone, Copy)]
pub struct SackpermMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> SackpermMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 2 {
            return Err(buf);
        }
        let container = Self { buf };
        if (container.header_len() as usize) != 2 {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        let header_len = self.header_len() as usize;
        &self.buf.as_ref()[header_len..]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.as_ref()[2..header_len]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.as_ref()[1])
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> SackpermMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len() as usize;
        &mut self.buf.as_mut()[header_len..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 2);
        &mut buf.as_mut()[..2].copy_from_slice(SACKPERM_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.as_mut()[2..header_len]
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 4);
        self.buf.as_mut()[0] = value;
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value == 2));
        self.buf.as_mut()[1] = (value);
    }
}

/// A fixed Sack header array.
pub const SACK_HEADER_ARRAY: [u8; 2] = [0x05, 0x0a];
#[derive(Debug, Clone, Copy)]
pub struct SackMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> SackMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 2 {
            return Err(buf);
        }
        let container = Self { buf };
        if ((container.header_len() as usize) < 2)
            || ((container.header_len() as usize) > remaining_len)
        {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        let header_len = self.header_len() as usize;
        &self.buf.as_ref()[header_len..]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.as_ref()[2..header_len]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> SackMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len() as usize;
        &mut self.buf.as_mut()[header_len..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 2);
        &mut buf.as_mut()[..2].copy_from_slice(SACK_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.as_mut()[2..header_len]
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 5);
        self.buf.as_mut()[0] = value;
    }
}

/// A fixed Ts header array.
pub const TS_HEADER_ARRAY: [u8; 10] = [0x08, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
#[derive(Debug, Clone, Copy)]
pub struct TsMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> TsMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 10 {
            return Err(buf);
        }
        let container = Self { buf };
        if (container.header_len() as usize) != 10 {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        let header_len = self.header_len() as usize;
        &self.buf.as_ref()[header_len..]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.as_ref()[10..header_len]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
    #[inline]
    pub fn ts(&self) -> u32 {
        NetworkEndian::read_u32(&self.buf.as_ref()[2..6])
    }
    #[inline]
    pub fn ts_echo(&self) -> u32 {
        NetworkEndian::read_u32(&self.buf.as_ref()[6..10])
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.as_ref()[1])
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> TsMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len() as usize;
        &mut self.buf.as_mut()[header_len..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 10);
        &mut buf.as_mut()[..10].copy_from_slice(TS_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.as_mut()[10..header_len]
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 8);
        self.buf.as_mut()[0] = value;
    }
    #[inline]
    pub fn set_ts(&mut self, value: u32) {
        NetworkEndian::write_u32(&mut self.buf.as_mut()[2..6], value);
    }
    #[inline]
    pub fn set_ts_echo(&mut self, value: u32) {
        NetworkEndian::write_u32(&mut self.buf.as_mut()[6..10], value);
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value == 10));
        self.buf.as_mut()[1] = (value);
    }
}

/// A fixed Fo header array.
pub const FO_HEADER_ARRAY: [u8; 18] = [
    0x22, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00,
];
#[derive(Debug, Clone, Copy)]
pub struct FoMessage<T> {
    buf: T,
}
impl<T: AsRef<[u8]>> FoMessage<T> {
    #[inline]
    pub fn parse_unchecked(buf: T) -> Self {
        Self { buf }
    }
    #[inline]
    pub fn buf(&self) -> &T {
        &self.buf
    }
    #[inline]
    pub fn release(self) -> T {
        self.buf
    }
    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 18 {
            return Err(buf);
        }
        let container = Self { buf };
        if (container.header_len() as usize) != 18 {
            return Err(container.buf);
        }
        Ok(container)
    }
    #[inline]
    pub fn payload(&self) -> &[u8] {
        let header_len = self.header_len() as usize;
        &self.buf.as_ref()[header_len..]
    }
    #[inline]
    pub fn option_slice(&self) -> &[u8] {
        let header_len = (self.header_len() as usize);
        &self.buf.as_ref()[18..header_len]
    }
    #[inline]
    pub fn type_(&self) -> u8 {
        self.buf.as_ref()[0]
    }
    #[inline]
    pub fn fo(&self) -> &[u8] {
        &self.buf.as_ref()[2..18]
    }
    #[inline]
    pub fn header_len(&self) -> u8 {
        (self.buf.as_ref()[1])
    }
}
impl<T: AsRef<[u8]> + AsMut<[u8]>> FoMessage<T> {
    #[inline]
    pub fn payload_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len() as usize;
        &mut self.buf.as_mut()[header_len..]
    }
    #[inline]
    pub fn build_message(buf: T) -> Self {
        assert!(buf.as_mut().len() >= 18);
        &mut buf.as_mut()[..18].copy_from_slice(FO_HEADER_ARRAY);
        Self { buf }
    }
    #[inline]
    pub fn option_slice_mut(&mut self) -> &mut [u8] {
        let header_len = (self.header_len() as usize);
        &mut self.buf.as_mut()[18..header_len]
    }
    #[inline]
    pub fn set_type_(&mut self, value: u8) {
        assert!(value == 34);
        self.buf.as_mut()[0] = value;
    }
    #[inline]
    pub fn set_fo(&mut self, value: &[u8]) {
        (&mut self.buf.as_mut()[2..18]).copy_from_slice(value);
    }
    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!((value == 18));
        self.buf.as_mut()[1] = (value);
    }
}

pub enum TcpOptGroup<T> {
    Eol_(Eol<T>),
    Nop_(Nop<T>),
    Mss_(Mss<T>),
    Wsopt_(Wsopt<T>),
    Sackperm_(Sackperm<T>),
    Sack_(Sack<T>),
    Ts_(Ts<T>),
    Fo_(Fo<T>),
}
impl<T: AsRef<[u8]>> TcpOptGroup<T> {
    pub fn group_parse(buf: T) -> Result<Self, T> {
        if buf.as_ref().len() < 1 {
            return Err(buf);
        }
        let cond_value = buf.as_ref()[0];
        match cond_value {
            0 => EolMessage::parse(buf).map(|msg| TcpOptGroup::EolMessage_(msg)),
            1 => NopMessage::parse(buf).map(|msg| TcpOptGroup::NopMessage_(msg)),
            2 => MssMessage::parse(buf).map(|msg| TcpOptGroup::MssMessage_(msg)),
            3 => WsoptMessage::parse(buf).map(|msg| TcpOptGroup::WsoptMessage_(msg)),
            4 => SackpermMessage::parse(buf).map(|msg| TcpOptGroup::SackpermMessage_(msg)),
            5 => SackMessage::parse(buf).map(|msg| TcpOptGroup::SackMessage_(msg)),
            8 => TsMessage::parse(buf).map(|msg| TcpOptGroup::TsMessage_(msg)),
            34 => FoMessage::parse(buf).map(|msg| TcpOptGroup::FoMessage_(msg)),
        }
    }
}
