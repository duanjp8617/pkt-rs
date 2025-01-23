use bytes::Buf;

use crate::{PktBuf, PktMut};

#[derive(Debug)]
pub struct Cursor<'a> {
    buf: &'a [u8],
    cursor: usize,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(buf: &'a [u8]) -> Self {
        Cursor { buf, cursor: 0 }
    }

    #[inline]
    pub fn original_buf(&self) -> &[u8] {
        self.buf
    }

    #[inline]
    pub fn current_buf(&self) -> &[u8] {
        &self.buf[self.cursor..]
    }

    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

// custom implementation for &[u8]
impl<'a> Buf for Cursor<'a> {
    #[inline]
    fn remaining(&self) -> usize {
        self.buf.len() - self.cursor
    }

    #[inline]
    fn chunk(&self) -> &[u8] {
        &self.buf[self.cursor..]
    }

    #[inline]
    fn advance(&mut self, cnt: usize) {
        assert!(cnt <= self.remaining());

        self.cursor += cnt;
    }
}

impl<'a> PktBuf for Cursor<'a> {
    #[inline]
    fn move_back(&mut self, cnt: usize) {
        assert!(cnt <= self.cursor);
        self.cursor -= cnt;
    }

    #[inline]
    fn trim_off(&mut self, cnt: usize) {
        assert!(cnt <= self.remaining());
        let truncate_idx = self.buf.len() - cnt;

        self.buf = &self.buf[..truncate_idx];
    }
}

#[derive(Debug)]
pub struct CursorMut<'a> {
    buf: &'a mut [u8],
    cursor: usize,
}

impl<'a> CursorMut<'a> {
    #[inline]
    pub fn new(buf: &'a mut [u8]) -> Self {
        CursorMut { buf, cursor: 0 }
    }

    #[inline]
    pub fn original_buf(&mut self) -> &mut [u8] {
        self.buf
    }

    #[inline]
    pub fn current_buf(&mut self) -> &mut [u8] {
        &mut self.buf[self.cursor..]
    }

    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

// custom implementation for &[u8]
impl<'a> Buf for CursorMut<'a> {
    #[inline]
    fn remaining(&self) -> usize {
        self.buf.as_ref().len() - self.cursor
    }

    #[inline]
    fn chunk(&self) -> &[u8] {
        &self.buf.as_ref()[self.cursor..]
    }

    #[inline]
    fn advance(&mut self, cnt: usize) {
        assert!(cnt <= self.remaining());

        self.cursor += cnt;
    }
}

impl<'a> PktBuf for CursorMut<'a> {
    #[inline]
    fn move_back(&mut self, cnt: usize) {
        assert!(cnt <= self.cursor);
        self.cursor -= cnt;
    }

    #[inline]
    fn trim_off(&mut self, cnt: usize) {
        assert!(cnt <= self.remaining());
        let truncate_idx = self.original_buf().len() - cnt;

        let original = std::mem::replace(&mut self.buf, &mut []);
        self.buf = original.split_at_mut(truncate_idx).0;
    }
}

impl<'a> PktMut for CursorMut<'a> {
    #[inline]
    fn chunk_mut(&mut self) -> &mut [u8] {
        &mut self.buf[self.cursor..]
    }

    #[inline]
    fn chunk_headroom(&self) -> usize {
        self.cursor
    }
}
