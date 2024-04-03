use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        FileIoError(io_err: std::io::Error) {
            display("file io error: {}", io_err)
            from()
        }
        MultByteChar(ch: char) {
            display("find a multi-byte character: {}", ch)
            from()
        }
    }
}

#[derive(Debug)]
pub struct FileText {
    path: PathBuf,
    text: String,
    lines: Vec<(usize, usize)>,
}

impl FileText {
    /// A source file that can render code blocks in the terminal.
    ///
    /// This is similar  to the FileText implementation in lalrpop,
    /// except that it does not accept files containing multi-byte characters.
    pub fn new<P: AsRef<Path>>(p: P) -> Result<Self, Error> {
        let mut f = File::open(p.as_ref())?;
        let mut text = String::new();
        f.read_to_string(&mut text)?;
        let lines = Self::analyze_lines(&text)?;

        Ok(Self {
            path: p.as_ref().to_path_buf(),
            text,
            lines,
        })
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    // calculate the offset of each line in the input file
    fn analyze_lines(text: &str) -> Result<Vec<(usize, usize)>, Error> {
        // we start by collecting the starting indexes of each line
        let line_idxes = std::iter::once(Ok(0))
            .chain(text.char_indices().filter_map(|(offset, c)| {
                if c.len_utf8() != 1 {
                    // find a char larger than 1 byte,
                    // abort the iteration by returning an Error
                    return Some(Err(Error::MultByteChar(c)));
                }

                if c == '\n' {
                    Some(Ok(offset + 1))
                } else {
                    None
                }
            }))
            .collect::<Result<Vec<_>, Error>>()?;

        // we then collect the offsets of each line
        let lines = line_idxes
            .iter()
            .enumerate()
            .filter_map(|(idx, line_idx)| {
                if idx + 1 < line_idxes.len() {
                    Some((*line_idx, line_idxes[idx + 1] - 1))
                } else {
                    None
                }
            })
            .chain(std::iter::once((*line_idxes.last().unwrap(), text.len())))
            .collect();

        Ok(lines)
    }

    /// Render the code block starting at byte offset `start`, and ending
    /// at byte offset `end`
    pub fn render_code_block(
        &self,
        start: usize,
        end: usize,
        out: &mut dyn Write,
    ) -> Result<(), Error> {
        assert!(start <= end, "invalid byte offset");

        let start_line_idx = self.line_idx(start);
        let end_line_idx = self.line_idx(end);
        assert!(
            start != self.lines[start_line_idx].1 && end != self.lines[end_line_idx].1,
            r#"can not render \n or eof"#
        );

        write!(
            out,
            "at {} {}:{}",
            self.path.to_str().unwrap(),
            start_line_idx + 1,
            start - self.lines[start_line_idx].0 + 1
        )?;
        if start_line_idx == end_line_idx {
            if start != end {
                writeln!(out, "-{}", end - self.lines[end_line_idx].0 + 1)?;
            }
            writeln!(out, "")?;

            self.highlight_line(start, end, out)?;
        } else {
            writeln!(
                out,
                "-{}:{}",
                end_line_idx + 1,
                end - self.lines[end_line_idx].0 + 1
            )?;
            self.highlight_block(start, end, out)?;
        }

        Ok(())
    }

    pub fn line_idx(&self, byte_offset: usize) -> usize {
        self.lines
            .binary_search_by(|(start, end)| {
                if byte_offset < *start {
                    std::cmp::Ordering::Greater
                } else if byte_offset > *end {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .expect("invalid byte offset")
    }

    fn highlight_line(&self, start: usize, end: usize, out: &mut dyn Write) -> Result<(), Error> {
        let line_idx = self.line_idx(start);
        let line_num_str = (line_idx + 1).to_string();

        write!(out, "{} | ", line_num_str)?;
        writeln!(
            out,
            "{}",
            &self.text[self.lines[line_idx].0..self.lines[line_idx].1]
        )?;
        keep_write(
            ' ',
            line_num_str.len() + 3 + start - self.lines[line_idx].0,
            out,
        )?;
        keep_write('^', end - start + 1, out)?;
        writeln!(out, "")?;

        Ok(())
    }

    // Render at most 6 lines.
    // If start - end > 5, only the first 3 lines and the last 3 lines
    // will be rendered, the lines in the middle will be omitted.
    fn highlight_block(&self, start: usize, end: usize, out: &mut dyn Write) -> Result<(), Error> {
        let start_lidx = self.line_idx(start);
        let end_lidx = self.line_idx(end);
        let end_line_num = (end_lidx).to_string();

        // Print the starting symbols.
        keep_write(
            ' ',
            end_line_num.len() + 3 + start - self.lines[start_lidx].0,
            out,
        )?;
        keep_write('~', self.lines[start_lidx].1 - start, out)?;
        writeln!(out, "")?;

        // A helper to render a line.
        let print_line = |line_idx: usize, out: &mut dyn Write| -> Result<(), Error> {
            let curr_line_num = (line_idx + 1).to_string();
            keep_write(' ', end_line_num.len() - curr_line_num.len(), out)?;
            writeln!(
                out,
                "{} | {}",
                curr_line_num,
                &self.text[self.lines[line_idx].0..self.lines[line_idx].1]
            )?;

            Ok(())
        };

        if end_lidx - start_lidx > 5 {
            // Render the first 3 lines.
            let _ = (start_lidx..start_lidx + 3)
                .into_iter()
                .map(|line_idx| print_line(line_idx, out))
                .collect::<Result<Vec<_>, Error>>()?;

            writeln!(out, "......",)?;

            // Render the last 3 lines.
            let _ = (end_lidx - 2..end_lidx + 1)
                .into_iter()
                .map(|line_idx| print_line(line_idx, out))
                .collect::<Result<Vec<_>, Error>>()?;
        } else {
            // Render all the lines
            let _ = (start_lidx..end_lidx + 1)
                .into_iter()
                .map(|line_idx| print_line(line_idx, out))
                .collect::<Result<Vec<_>, Error>>()?;
        }

        // Print the ending symbols.
        keep_write(' ', end_line_num.len() + 3, out)?;
        keep_write('^', end - self.lines[end_lidx].0 + 1, out)?;
        writeln!(out, "")?;

        Ok(())
    }
}

fn keep_write(c: char, times: usize, out: &mut dyn Write) -> Result<(), Error> {
    for _ in 0..times {
        write!(out, "{}", c)?;
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bad_file_path() {
        let res = FileText::new("sss**xx");
        assert_eq!(
            "file io error: No such file or directory (os error 2)",
            &format!("{}", res.unwrap_err())
        );
    }

    #[test]
    fn multi_byte_char() {
        let content = r#"a haha
        wtf你是什么？"#;
        let res = FileText::analyze_lines(content);

        assert_eq!(
            "find a multi-byte character: 你",
            &format!("{}", res.unwrap_err())
        );
    }

    #[test]
    fn parse_lines() {
        let content = r#"1
23

456

7890
"#;
        let lines = FileText::analyze_lines(content).unwrap();

        assert_eq!("1", &content[lines[0].0..lines[0].1]);
        assert_eq!("23", &content[lines[1].0..lines[1].1]);
        assert_eq!("", &content[lines[2].0..lines[2].1]);
        assert_eq!("456", &content[lines[3].0..lines[3].1]);
        assert_eq!("", &content[lines[4].0..lines[4].1]);
        assert_eq!("7890", &content[lines[5].0..lines[5].1]);
        assert_eq!("", &content[lines[6].0..lines[6].1]);
    }
}
