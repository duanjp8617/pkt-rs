use byteorder::{ByteOrder, NetworkEndian};

const END_OF_LIST: u8 = 0;
const NOP: u8 = 1;
const MAX_SEG_SIZE: u8 = 2;
const WINDOW_SCALE: u8 = 3;
const SACK_PERMITTED: u8 = 4;
const SELECTIVE_ACK: u8 = 5;
const TIMESTAMPS: u8 = 8;
const TCP_FASTOPEN: u8 = 34;

///
/// sub_type Eol {
///   kind: TcpOption::Eol,
///   type: Bit(8), Mult(1) | Start(0), End(Some(0)) | Repr(u8), Arg(u8) | Default(0), 
/// }
/// 
/// sub_type Nop {
///    kind: TcpOption:Nop, 
///    type: Bit(8), Mult(1) | Start(1), End(Some(1)) | Repr(u8), Arg(u8) | Default(1), 
/// }
/// 
/// sub_type Mss {
///    kind: TcpOption::Mss,
///    type: Bit(8), Mult(1) | Start(2), End(Some(2)) | Repr(u8), Arg(u8) | Default(2), 
///    header_len: Bit(8), Mult(1) | Start(4), End(Some(4)) | Repr(u8), Arg(u8) | Default(4), 
///    mss: Bit(16), Mult(1) | Start(0), End(None) | Repr(u16), Arg(u16) | Default(0),
/// }
/// 
/// sub_type Wsopt {
///    kind: TcpOption::Wsopt,
///    type: Bit(8), Mult(1) | Start(3), End(Some(3)) | Repr(u8), Arg(u8) | Default(3), 
///    header_len: Bit(8), Mult(1) | Start(3), End(Some(3)) | Repr(u8), Arg(u8) | Default(3), 
///    wsopt: Bit(8), Mult(1) | Start(0), End(None) | Repr(u8), Arg(u8) | Default(0),
/// }
/// 
/// 
pub enum TcpOption<'a> {
    Eol,
    Nop,
    Mss(u16),
    Wsopt(u8),
    SackPerm,
    Sack(TcpOptionSack<&'a [u8]>),
    Ts(u32, u32),
    Fo(&'a [u8]),
    Unknown(&'a [u8]),
}

pub struct TcpOptionSack<T> {
    buf: T,
}

impl<T: AsRef<[u8]>> TcpOptionSack<T> {
    #[inline]
    pub fn num_blocks(&self) -> usize {
        (usize::from(self.buf.as_ref()[1]) - 2) / 8
    }

    #[inline]
    pub fn block(&self, idx: usize) -> (u32, u32) {
        assert!(idx < self.num_blocks());
        (
            NetworkEndian::read_u32(&self.buf.as_ref()[2 + 8 * idx..6 + 8 * idx]),
            NetworkEndian::read_u32(&self.buf.as_ref()[6 + 8 * idx..10 + 8 * idx]),
        )
    }
}

impl<T: AsMut<[u8]> + AsRef<[u8]>> TcpOptionSack<T> {
    #[inline]
    pub fn set_block(&mut self, idx: usize, sack: (u32, u32)) {
        assert!(idx < self.num_blocks());
        NetworkEndian::write_u32(&mut self.buf.as_mut()[2 + 8 * idx..6 + 8 * idx], sack.0);
        NetworkEndian::write_u32(&mut self.buf.as_mut()[6 + 8 * idx..10 + 8 * idx], sack.1);
    }
}

pub struct TcpOptionWriter<'a> {
    buf: &'a mut [u8],
}

impl<'a> TcpOptionWriter<'a> {
    pub fn eol(&mut self) {
        self.buf[0] = END_OF_LIST;

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(1);
        self.buf = remaining;
    }

    pub fn nop(&mut self) {
        self.buf[0] = NOP;

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(1);
        self.buf = remaining;
    }

    pub fn mss(&mut self, mss: u16) {
        self.buf[0] = MAX_SEG_SIZE;
        self.buf[1] = 4;
        NetworkEndian::write_u16(&mut self.buf[2..4], mss);

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(4);
        self.buf = remaining;
    }

    pub fn wsopt(&mut self, value: u8) {
        self.buf[0] = WINDOW_SCALE;
        self.buf[1] = 3;
        self.buf[2] = value;

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(3);
        self.buf = remaining;
    }

    pub fn sack_perm(&mut self) {
        self.buf[0] = SACK_PERMITTED;
        self.buf[1] = 2;

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(2);
        self.buf = remaining;
    }

    pub fn sack(&mut self, num_blocks: usize) -> TcpOptionSack<&'a mut [u8]> {
        assert!(num_blocks <= 4);
        let opt_len = 2 + num_blocks * 8;

        self.buf[0] = SELECTIVE_ACK;
        self.buf[1] = opt_len as u8;

        let (buf, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(opt_len);
        self.buf = remaining;

        TcpOptionSack { buf }
    }

    pub fn ts(&mut self, ts: u32, ts_echo: u32) {
        self.buf[0] = TIMESTAMPS;
        self.buf[1] = 10;
        NetworkEndian::write_u32(&mut self.buf[2..6], ts);
        NetworkEndian::write_u32(&mut self.buf[6..10], ts_echo);

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(10);
        self.buf = remaining;
    }

    pub fn fo(&mut self, cookie: &[u8]) {
        self.buf[0] = TCP_FASTOPEN;
        self.buf[1] = 18;
        (&mut self.buf[2..18]).copy_from_slice(cookie);

        let (_, remaining) = std::mem::replace(&mut self.buf, &mut []).split_at_mut(18);
        self.buf = remaining;
    }

    #[inline]
    pub fn from_option_bytes_mut(buf: &'a mut [u8]) -> Self {
        Self { buf }
    }

    #[inline]
    pub fn remaining_bytes(&self) -> usize {
        self.buf.len()
    }
}

pub struct TcpOptionIter<'a> {
    buf: &'a [u8],
    valid: bool,
}

impl<'a> TcpOptionIter<'a> {
    #[inline]
    pub fn from_option_bytes(buf: &'a [u8]) -> TcpOptionIter<'a> {
        Self { buf, valid: true }
    }

    #[inline]
    pub fn check_option_bytes(buf: &'a [u8]) -> bool {
        let mut reader = Self::from_option_bytes(buf);
        while let Some(_) = (&mut reader).next() {}
        reader.valid
    }
}

impl<'a> Iterator for TcpOptionIter<'a> {
    type Item = TcpOption<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.valid || self.buf.len() == 0 {
            return None;
        }

        let opt_type = self.buf[0];
        match opt_type {
            END_OF_LIST => {
                self.buf = &self.buf[1..];
                if self.buf.len() > 0 {
                    self.valid = false;
                }
                Some(TcpOption::Eol)
            }
            NOP => {
                self.buf = &self.buf[1..];
                Some(TcpOption::Nop)
            }
            _ => {
                if self.buf.len() < 2 {
                    self.valid = false;
                    return None;
                }

                match opt_type {
                    MAX_SEG_SIZE => {
                        if self.buf[1] != 4 || self.buf.len() < 4 {
                            self.valid = false;
                            None
                        } else {
                            let mss = NetworkEndian::read_u16(&self.buf[2..4]);
                            self.buf = &self.buf[4..];
                            Some(TcpOption::Mss(mss))
                        }
                    }
                    WINDOW_SCALE => {
                        if self.buf[1] != 3 || self.buf.len() < 3 {
                            self.valid = false;
                            None
                        } else {
                            let ws = self.buf[2];
                            self.buf = &self.buf[3..];
                            Some(TcpOption::Wsopt(ws))
                        }
                    }
                    SACK_PERMITTED => {
                        if self.buf[1] != 2 || self.buf.len() < 2 {
                            self.valid = false;
                            None
                        } else {
                            self.buf = &self.buf[2..];
                            Some(TcpOption::SackPerm)
                        }
                    }
                    SELECTIVE_ACK => {
                        let opt_len = usize::from(self.buf[1]);

                        #[inline]
                        fn opt_len_valid(len: usize) -> bool {
                            len >= 2 && len <= 40 && (len - 2) % 8 == 0
                        }

                        if !opt_len_valid(opt_len) || self.buf.len() < opt_len {
                            self.valid = false;
                            None
                        } else {
                            let opt = TcpOptionSack {
                                buf: &self.buf[..opt_len],
                            };
                            self.buf = &self.buf[opt_len..];
                            Some(TcpOption::Sack(opt))
                        }
                    }
                    TIMESTAMPS => {
                        if self.buf[1] != 10 || self.buf.len() < 10 {
                            self.valid = false;
                            None
                        } else {
                            let ts = NetworkEndian::read_u32(&self.buf[2..6]);
                            let ts_echo = NetworkEndian::read_u32(&self.buf[6..10]);
                            self.buf = &self.buf[10..];
                            Some(TcpOption::Ts(ts, ts_echo))
                        }
                    }
                    TCP_FASTOPEN => {
                        if self.buf[1] != 18 || self.buf.len() < 18 {
                            self.valid = false;
                            None
                        } else {
                            let buf = &self.buf[2..18];
                            self.buf = &self.buf[18..];
                            Some(TcpOption::Fo(buf))
                        }
                    }
                    _ => {
                        let opt_len = usize::from(self.buf[1]);
                        if self.buf.len() < opt_len {
                            self.valid = false;
                            None
                        } else {
                            let opt = &self.buf[..opt_len];
                            self.buf = &self.buf[opt_len..];
                            Some(TcpOption::Unknown(opt))
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static OPTIONS: [u8; 70] = [
        0x01, 0x02, 0x04, 0x05, 0xdc, 0x03, 0x03, 0x0c, 0x4, 0x02, 0x05, 0x0a, 0x00, 0x00, 0x01,
        0xf4, 0x00, 0x00, 0x05, 0xdc, 0x05, 0x12, 0x00, 0x00, 0x03, 0x6b, 0x00, 0x00, 0x04, 0xc9,
        0x00, 0x00, 0x05, 0xdc, 0x00, 0x00, 0x09, 0xc4, 0x05, 0x1a, 0x00, 0x0d, 0x59, 0xf8, 0x00,
        0x12, 0xb1, 0x28, 0x00, 0x16, 0xe3, 0x60, 0x00, 0x26, 0x25, 0xa0, 0x34, 0x3e, 0xfc, 0xea,
        0x34, 0x40, 0xae, 0xf0, 0x0c, 0x05, 0x01, 0x02, 0x03, 0x00,
    ];

    #[test]
    fn option_parse() {
        let mut opt_iter = TcpOptionIter::from_option_bytes(&OPTIONS[..]);

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Nop = opt {
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Mss(mss) = opt {
                assert_eq!(mss, 1500);
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Wsopt(ws) = opt {
                assert_eq!(ws, 12);
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::SackPerm = opt {
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Sack(sack) = opt {
                assert_eq!(sack.num_blocks(), 1);
                assert_eq!(sack.block(0), (500, 1500));
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Sack(sack) = opt {
                assert_eq!(sack.num_blocks(), 2);
                assert_eq!(sack.block(0), (875, 1225));
                assert_eq!(sack.block(1), (1500, 2500));
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Sack(sack) = opt {
                assert_eq!(sack.num_blocks(), 3);
                assert_eq!(sack.block(0), (875000, 1225000));
                assert_eq!(sack.block(1), (1500000, 2500000));
                assert_eq!(sack.block(2), (876543210, 876654320));
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Unknown(b) = opt {
                assert_eq!(b, &[0x0c, 0x05, 0x01, 0x02, 0x03]);
                true
            } else {
                false
            },
            true
        );

        let opt = opt_iter.next().unwrap();
        assert_eq!(
            if let TcpOption::Eol = opt {
                true
            } else {
                false
            },
            true
        );

        assert_eq!(TcpOptionIter::check_option_bytes(&OPTIONS[..]), true);
    }

    #[test]
    fn option_build() {
        let mut buf: [u8; 70] = [0; 70];
        let mut opt_writer = TcpOptionWriter::from_option_bytes_mut(&mut buf[..]);

        opt_writer.nop();
        opt_writer.mss(1500);
        opt_writer.wsopt(12);
        opt_writer.sack_perm();
        opt_writer.sack(1).set_block(0, (500, 1500));

        let mut sack = opt_writer.sack(2);
        sack.set_block(0, (875, 1225));
        sack.set_block(1, (1500, 2500));

        let mut sack = opt_writer.sack(3);
        sack.set_block(0, (875000, 1225000));
        sack.set_block(1, (1500000, 2500000));
        sack.set_block(2, (876543210, 876654320));

        assert_eq!(&buf[..64], &OPTIONS[..64]);
    }
}
