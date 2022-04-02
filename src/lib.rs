use ansi_term::Color;
use ansi_term::Color::Fixed;
use ansi_term::Style;
use regex::Regex;
use serde::{Serialize, Deserialize};
use std::cmp::min;
use std::collections::HashSet;
use std::convert::From;
use std::env;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Write;
use std::num::NonZeroUsize;
use std::path::Path;
use std::path::PathBuf;
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

/* Byte formatting stuff lifted from hexyl */
pub enum ByteCategory {
    Null,
    AsciiPrintable,
    AsciiWhitespace,
    AsciiOther,
    NonAscii,
}

#[derive(Copy, Clone)]
pub struct Byte(pub u8);

impl Byte {
    pub fn category(self) -> ByteCategory {
        if self.0 == 0x00 {
            ByteCategory::Null
        } else if self.0.is_ascii_graphic() {
            ByteCategory::AsciiPrintable
        } else if self.0.is_ascii_whitespace() {
            ByteCategory::AsciiWhitespace
        } else if self.0.is_ascii() {
            ByteCategory::AsciiOther
        } else {
            ByteCategory::NonAscii
        }
    }

    pub fn color(self) -> &'static Color {
        use ByteCategory::*;

        match self.category() {
            Null => &COLOR_NULL,
            AsciiPrintable => &COLOR_ASCII_PRINTABLE,
            AsciiWhitespace => &COLOR_ASCII_WHITESPACE,
            AsciiOther => &COLOR_ASCII_OTHER,
            NonAscii => &COLOR_NONASCII,
        }
    }

    pub fn as_char(self) -> char {
        use ByteCategory::*;

        match self.category() {

            /* hexyl uses 0 here, depending on color to distinguish */
            Null => '•',
            AsciiPrintable => self.0 as char,
            AsciiWhitespace if self.0 == 0x20 => ' ',
            AsciiWhitespace => '_',
            AsciiOther => '•',
            NonAscii => '×',
        }
    }
}

pub const COLOR_NULL: Color = Fixed(1);
pub const COLOR_ASCII_PRINTABLE: Color = Color::Cyan;
pub const COLOR_ASCII_WHITESPACE: Color = Color::Green;
pub const COLOR_ASCII_OTHER: Color = Color::Purple;
pub const COLOR_NONASCII: Color = Color::Yellow;


/// Per [XDG Base Directory Specification], dotfiles should go to
/// $XDG_CONFIG_HOME if it exists, and $HOME/.config if it doesn't.
///
/// [XDG Base Directory Specification]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
pub fn preferences_file_path() -> PathBuf {
    let xdg_config_home = env::var("XDG_CONFIG_HOME");
    let home = env::var("HOME");

    let edhex_s = String::from("edhex");
    let prefs_s = String::from("preferences");

    if xdg_config_home.is_ok() {
        let mut return_if_good = PathBuf::from(xdg_config_home.unwrap());

        /* Spec says it must be absolute or ignored */
        if return_if_good.is_absolute() {
            return_if_good.push(edhex_s);
            return_if_good.push(prefs_s);
            return return_if_good;
        }
    }

    let dotconf_s = String::from(".config");
    if home.is_ok() {
        [home.unwrap(), dotconf_s, edhex_s, prefs_s].iter().collect()
    }
    else {
        [".".to_owned(), dotconf_s, edhex_s, prefs_s].iter().collect()
    }
}


/// Per [XDG Base Directory Specification], state files should go to
/// $XDG_STATE_HOME if it exists, and $HOME/.local/state if it doesn't.
///
/// [XDG Base Directory Specification]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
pub fn state_file_path() -> PathBuf {
    let xdg_state_home = env::var("XDG_STATE_HOME");
    let home = env::var("HOME");

    let edhex_s = String::from("edhex");
    let state_s = String::from("state");

    if xdg_state_home.is_ok() {
        let mut return_if_good = PathBuf::from(xdg_state_home.unwrap());

        /* Spec says it must be absolute or ignored */
        if return_if_good.is_absolute() {
            return_if_good.push(edhex_s);
            return_if_good.push(state_s);
            return return_if_good;
        }
    }

    let dotlocal_s = String::from(".local");
    if home.is_ok() {
        [home.unwrap(), dotlocal_s, state_s, edhex_s].iter().collect()
    }
    else {
        [".".to_owned(), dotlocal_s, state_s, edhex_s].iter().collect()
    }
}


/// TODO Per https://doc.rust-lang.org/std/io/enum.ErrorKind.html
/// IsADirectory isn't available yet.  Check against it when it is.
pub fn is_a_regular_file(filename: &str) -> bool {
    let path = Path::new(filename);
    path.is_file()
}


pub fn path_exists(filename: &str) -> bool {
    let path = Path::new(filename);
    path.exists()
}


pub fn num_bytes_or_die(open_file: &Option<std::fs::File>) -> Result<usize, i32> {
    if open_file.is_none() {
        return Ok(0);
    }

    let metadata = open_file.as_ref().unwrap().metadata();
    match metadata {
        Ok(metadata) => {
            Ok(metadata.len() as usize)
        }
        Err(_) => {
            println!("Couldn't find file size");
            Err(2)
        }
    }
}


#[derive(Error, Debug)]
pub enum AllBytesFromFilenameError {
    #[error("Cannot read file")]
    FileCannotBeRead,
    #[error("File does not exist")]
    FileDoesNotExist,
    #[error("File is not a regular file")]
    NotARegularFile,
    #[error("Cannot read all bytes of file")]
    CantReadAllBytes,
}


pub fn all_bytes_from_filename(filename: &str)
        -> Result<Vec<u8>, AllBytesFromFilenameError> {

    /* As written right now, `file` always is Some */
    let file = match filehandle(filename) {
        Ok(Some(filehandle)) => {
            Some(filehandle)
        },
        Ok(None) => {
            return Err(AllBytesFromFilenameError::FileDoesNotExist);
        },
        Err(_) => {
            return Err(AllBytesFromFilenameError::FileCannotBeRead);
        }
    };

    let original_num_bytes = match num_bytes_or_die(&file) {
        Ok(num_bytes) => {
            num_bytes
        },
        Err(_) => {
            return Err(AllBytesFromFilenameError::FileCannotBeRead);
        }
    };

    /* Read all bytes into memory just like real ed */
    // TODO A real hex editor needs to buffer
    let mut all_bytes = Vec::new();
    if file.is_some() {
        match file.unwrap().read_to_end(&mut all_bytes) {
            Err(_) => {
                if path_exists(filename) {
                    if !is_a_regular_file(filename) {
                        Err(AllBytesFromFilenameError::NotARegularFile)
                    }
                    else {
                        Err(AllBytesFromFilenameError::FileCannotBeRead)
                    }
                }
                else {
                    Err(AllBytesFromFilenameError::FileDoesNotExist)
                }
            },
            Ok(num_bytes_read) => {
                if num_bytes_read != original_num_bytes {
                    Err(AllBytesFromFilenameError::CantReadAllBytes)
                }
                else {
                    Ok(all_bytes)
                }
            }
        }
    }
    else {
        Err(AllBytesFromFilenameError::FileDoesNotExist)
    }
}



/// This struct exists exclusively for serializing State's to disk without
/// including all_bytes.  If more entries end up in State that need to be
/// saved off, change this struct to reflect them.
///
/// Attempts to deserialize from one of these will cause you to notice
/// missing keys if State changes without you updating this.
#[derive(Serialize, Deserialize, Debug)]
pub struct StateSansBytes {
    pub prefs: Preferences,
    pub unsaved_changes: bool,
    pub filename: String,
    pub readonly: bool,
    pub last_search: Option<Vec<u8>>,
    pub index: usize,
    pub breaks: HashSet<usize>,
}


impl From<&State> for StateSansBytes {
    fn from(state: &State) -> Self {
        StateSansBytes {
            prefs: state.prefs.clone(),
            unsaved_changes: state.unsaved_changes,
            filename: state.filename.clone(),
            readonly: state.readonly,
            last_search: state.last_search.clone(),
            index: state.index,
            breaks: state.breaks.clone(),
        }
    }
}


impl From<&StateSansBytes> for State {
    fn from(state_sans_bytes: &StateSansBytes) -> Self {
        State {
            prefs: state_sans_bytes.prefs.clone(),
            unsaved_changes: state_sans_bytes.unsaved_changes,
            filename: state_sans_bytes.filename.clone(),
            readonly: state_sans_bytes.readonly,
            index: state_sans_bytes.index,
            last_search: state_sans_bytes.last_search.clone(),
            all_bytes: vec![],
            breaks: state_sans_bytes.breaks.clone(),
        }
    }
}


#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Preferences {
    pub radix: u32,
    pub show_byte_numbers: bool,
    pub show_chars: bool,
    pub show_prompt: bool,
    pub color: bool,
    pub width: NonZeroUsize,

    /* Spaces to put between a byte number and a byte when displaying */
    pub n_padding: String,

    /* Number of lines to print before current line */
    pub before_context: usize,

    /* Number of lines to print after current line */
    pub after_context: usize,
}


pub trait DiskWritable {
    fn write_to_disk(self, filename: &str) -> Result<(), String>;
}


impl DiskWritable for &Preferences {
    fn write_to_disk(self, filename: &str) -> Result<(), String> {
        let serialized = serde_json::to_string_pretty(&self);

        if serialized.is_err() {
            return Err("? Could not serialize preferences.".to_owned());
        }
        let serialized = serialized.unwrap();

        let result = std::fs::write(filename, &serialized);

        if result.is_err() {
            Err(format!("? Couldn't write to {}", filename))
        }
        else {
            Ok(())
        }
    }
}


impl Preferences {
    pub fn read_from_filename(filename: &str) -> Result<Preferences, String> {
        Self::read_from_path(Path::new(filename))
    }


    pub fn read_from_path(path: &Path) -> Result<Preferences, String> {
        if let Ok(from_disk) = fs::read_to_string(path) {
            serde_json::from_str(&from_disk).or_else(
                |_| Err(format!("Could not read preferences from {}",
                        path.display()))
            )
        }
        else {
            Err(format!("Couldn't read {}", path.display()))
        }
    }


    pub fn default() -> Self {
        Self {
            radix: 16,
            show_byte_numbers: true,
            show_prompt: true,
            color: true,
            show_chars: true,
            before_context: 0,
            after_context: 0,
            width: NonZeroUsize::new(16).unwrap(),
            // TODO calculate based on longest possible index
            n_padding: "      ".to_owned(),
        }
    }
}


pub struct State {
    pub prefs: Preferences,
    pub unsaved_changes: bool,
    pub filename: String,
    pub readonly: bool,
    pub last_search: Option<Vec<u8>>,

    /* Current byte number, 0 to (len - 1) */
    pub index: usize,

    /* The bytes in memory */
    pub all_bytes: Vec<u8>,

    /* Bytes at which to insert a break when displaying */
    pub breaks: HashSet<usize>,
}


pub fn lino(state:&State) -> String {
    hex_unless_dec_with_radix(state.index, state.prefs.radix)
}


pub fn string_from_radix(radix: u32) -> String {
    if radix == 10 {
        "decimal".to_owned()
    }
    else {
        "hex".to_owned()
    }
}


pub fn hex_unless_dec(number:usize, radix:u32) -> String {
    if radix == 10 {
        format!("{}", number)
    }
    else {
        format!("{:x}", number)
    }
}


pub fn hex_unless_dec_with_radix(number:usize, radix:u32) -> String {
    let letter = if radix == 10 {
        'd'
    }
    else {
        'x'
    };

    format!("0{}{}", letter, hex_unless_dec(number, radix))
}


impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "radix: {}|unsaved_changes: {}|show_byte_numbers: {}|show_chars: {}|index: {}|width: {}|n_padding: '{}'|filename: {}|breaks: {:?}",
                self.prefs.radix, self.unsaved_changes, self.prefs.show_byte_numbers,
                self.prefs.show_chars, self.index, self.prefs.width, self.prefs.n_padding,
                self.filename, self.breaks)
    }
}


impl DiskWritable for &State {
    /// Serialize to a json blob.
    /// Don't include all_bytes, not only because it could be huge, but
    /// because if you load a state from disk and its bytes differ from
    /// the actual file's on disk bytes, neither collection of bytes are
    /// canonical.  User's can write out bytes to other files at will.
    fn write_to_disk(self, filename: &str) -> Result<(), String> {

        /* NOTICE: To exclude all_bytes, we have to list all the keys
        *  we want to save.  There is no reasonable, general way to
        *  iterate through all keys in a struct.
        *  When State changes what you'd want to save off, you have to
        *  change what this function saves off, too. */
        let serialized = serde_json::to_string_pretty(&StateSansBytes::from(self));

        if serialized.is_err() {
            return Err("? Could not serialize state.".to_owned());
        }
        let serialized = serialized.unwrap();

        let result = std::fs::write(filename, &serialized);

        if result.is_err() {
            Err(format!("? Couldn't write to {}", filename))
        }
        else {
            Ok(())
        }
    }
}


impl State {
    pub fn pretty_color_state(&self) -> String {
        if self.prefs.color {
            format!("{}{}{}{}{}", Color::Red.paint("c"),
                    Color::Yellow.paint("o"), Color::Green.paint("l"),
                    Color::Blue.paint("o"), Color::Purple.paint("r"))
        }
        else {
            "color".to_owned()
        }
    }


    /// Return the byte numbers necessary for the left column of a display
    pub fn addresses(&self, from:usize) -> Vec<usize> {
        // TODO Do this with a generator once
        // those are supported in Rust
        let mut to_return:Vec<usize> = vec![];
        for i in (from..self.all_bytes.len()).step_by(usize::from(self.prefs.width)) {
            to_return.push(i);
        }
        to_return
    }


    pub fn read_from_filename(filename: &str) -> Result<Self, String> {
        Self::read_from_path(Path::new(filename))
    }


    /// Note:  For the forseeable future, from_reader is actually slower
    /// than drawing the entire file into memory.  So until that changes,
    /// doing the latter:  https://github.com/serde-rs/json/issues/160
    pub fn read_from_path(path: &Path) -> Result<Self, String> {
        let state_sans_bytes_s = fs::read_to_string(path);
        if state_sans_bytes_s.is_err() {
            return Err(format!("Problem opening '{}' ({:?})", path.display(),
                    state_sans_bytes_s));
        }
        let state_sans_bytes_s = state_sans_bytes_s.unwrap();
        let state_sans_bytes_r = serde_json::from_str(&state_sans_bytes_s);
        if state_sans_bytes_r.is_err() {
            return Err(format!("? {:?}", state_sans_bytes_r));
        }
        let state_sans_bytes: StateSansBytes = state_sans_bytes_r.unwrap();

        let all_bytes = all_bytes_from_filename(&state_sans_bytes.filename); 
        if all_bytes.is_ok() {
            let mut to_return = State::from(&state_sans_bytes);
            to_return.all_bytes = all_bytes.unwrap();
            Ok(to_return)
        }
        else {
            Err(format!(
                "State in file '{}' said to find the bytes in '{}', but
                    couldn't read '{}'.", path.display(), state_sans_bytes.filename,
                    state_sans_bytes.filename))
        }
    }


    /// Return the range `self.prefs.width` bytes starting at `address`.
    /// Could be cut short by hitting end of all bytes.
    /// Could be empty because `address` is past end of all bytes.
    pub fn bytes_range_from(&self, address:usize) -> std::ops::Range<usize> {
        let width = usize::from(self.prefs.width);
        if address < self.all_bytes.len() {
            let end_index = min(self.all_bytes.len(), address + width);
            address..end_index
        }
        else {
            0..0
        }
    }


    /// Print `self.prefs.width` bytes from `address` or cut off if at end of all bytes
    pub fn bytes_from(&self, address:usize) -> &[u8] {
        &self.all_bytes[self.bytes_range_from(address)]
    }


    pub fn bytes_from_current_row(&self) -> &[u8] {
        self.bytes_from(self.index)
    }


    pub fn current_row_string(&self) -> String {
        self.bytes_line(self.bytes_from_current_row(), 0, false)
    }


    /// Returns the line to print and the index of the last byte printed
    pub fn line_with_break(&self, begin:usize, end:usize, underline:bool)
        -> Option<(String, usize)> {
      if end < begin {
        return None;
      }

      let all_bytes_length = self.all_bytes.len();
      if all_bytes_length == 0 {
        return None;
      }
      if begin + 1 > all_bytes_length {
        return None;
      }

      let mut to_return = String::new();

      /* Address if present */
      if self.prefs.show_byte_numbers {
        to_return += &format!("{}|", &address_display(begin, self.prefs.radix,
            &self.prefs.n_padding, underline));
      }

      /* Bytes */

      /* First byte to print, ignore if it's a break byte.
      *  Guaranteed not to be off the edge because of length check above. */
      to_return += &formatted_byte(self.all_bytes[begin], self.prefs.color,
          underline);
      to_return += " ";
      let mut num_shown = 1;

      for index in (begin + 1)..min(end + 1, self.all_bytes.len()) {
        if (self.breaks.contains(&index)) ||
            ((num_shown + 1) > usize::from(self.prefs.width)) {
          break;
        }

        to_return += &formatted_byte(self.all_bytes[index], self.prefs.color,
            underline);
        to_return += " ";
        num_shown += 1;
      }

      /* Remove trailing space */
      to_return.pop();

      /* Pad for unprinted bytes */
      for _ in 0..(usize::from(self.prefs.width).saturating_sub(num_shown)) {
        to_return += "   ";
      }

      /* Chars */

      if self.prefs.show_chars {
        to_return += &format!("|   ");

        to_return += &formatted_char(self.all_bytes[begin], self.prefs.color,
            underline);
        let mut num_chars_shown = 1;

        for index in (begin + 1)..min(end + 1, self.all_bytes.len()) {
          if (self.breaks.contains(&index)) ||
              ((num_chars_shown + 1) > usize::from(self.prefs.width)) {
            break;
          }

          to_return += &formatted_char(self.all_bytes[index], self.prefs.color,
              underline);
          num_chars_shown += 1;
        }
      }

      Some((to_return, begin + num_shown - 1))
    }


    pub fn bytes_line(&self, bytes:&[u8], line_number:usize,
            underline:bool) -> String {
        let join_char = if underline {
            " "
        }
        else {
            " "
        };
        bytes_line_bytes(bytes, line_number, self.prefs.width).iter().map(|x| formatted_byte(*x, self.prefs.color, underline)).collect::<Vec<String>>().join(join_char)
    }


    // TODO Do this padding stuff format!  Unclear why previous attempts
    // have failed.
    pub fn bytes_line_padding(&self, bytes:&[u8], line_num:usize) -> String{
        let mut to_return = String::new();
        let expected_length = usize::from(self.prefs.width) * 3 - 1;
        let actual_length = bytes_line_bytes(bytes, line_num,
                self.prefs.width).len() * 3 - 1;
        for _ in actual_length..expected_length {
            to_return += " ";
        }

        to_return
    }


    /// Return the range for the bytes in the current row
    pub fn range_from_current_row(&self) ->
            Result<(usize, usize), String> {
        let last_index = self.last_byte_of_row_index();
        if last_index.is_err() {
            return Err(format!("{:?}", last_index));
        }
        Ok((self.index, last_index.unwrap()))
    }


    /// In current row, what's the index of the last byte
    /// to show?
    pub fn last_byte_of_row_index(&self) -> Result<usize, String> {
        let max = self.max_index();
        if max.is_err() {
            return Err(format!("{:?}", max));
        }
        let max = max.unwrap();

        Ok(min(max, self.index + usize::from(self.prefs.width) - 1))
    }


    pub fn print_bytes_and_move_index(&mut self) {
        if let Some(last_byte_index) = self.print_bytes(true) {
            if let Ok(max) = self.max_index() {
                let new_index = min(last_byte_index + 1, max);
                self.index = new_index;
            }
            else {
                println!("? (No bytes)");
            }
        }
        else {
            println!("? (unknown error)");
        }
    }


    pub fn index_of_byte_after(&self, index:usize) -> Option<usize> {
        let would_be = index + 1;
        if let Ok(max) = self.max_index() {
            if would_be > max {
                None
            }
            else {
                Some(would_be)
            }
        }
        else {
            None
        }
    }



    pub fn index_of_next_byte(&self) -> Option<usize> {
        self.index_of_byte_after(self.index)
    }

    
    pub fn index_of_next_line(&self) -> Option<usize> {

        // TODO Replace this with direct calculation, then
        // call it inside line_with_break
        /* Calculate line_with_break only to get last_byte_index */
        if let Some((_, last_byte_index)) = self.line_with_break(self.index,
                self.all_bytes.len().saturating_sub(1), false) {
            if let Ok(max) = self.max_index() {
                Some(min(last_byte_index + 1, max))
            }
            else {
                None
            }
        }
        else {
            None
        }
    }


    pub fn move_index_then_print_bytes(&mut self) {
        if let Some(next_index) = self.index_of_next_line() {
            self.index = next_index;
            self.print_bytes(true);
        }
        else {
            println!("? No bytes after current line");
        }
    }


    /// returns index of the last byte printed in the non-context line
    pub fn print_bytes(&self, underline_main_bytes:bool) -> Option<usize> {
        if self.empty() {
            return None;
        }

        let max = self.max_index();
        if max.is_err() {
            println!("? ({:?})", max);
            return None;
        }
        let max = max.unwrap();

        /* Print the main line, underlined */
        if let Some((line, last_byte_index)) = self.line_with_break(self.index,
                self.all_bytes.len().saturating_sub(1), underline_main_bytes) {
            println!("{}", line);
            Some(last_byte_index)
        }
        else {
            None
        }
    }


    pub fn byte_indices_between(&self, range:(usize, usize)) ->
            Option<(usize, usize)> {
        if self.empty() {
            None
        }
        else if let Ok(max) = self.max_index() {
            if range.0 > max {
                None
            }
            else {
                Some((range.0, min(max, range.1)))
            }
        }
        else {
            None
        }
    }


    pub fn bytes_in_range(&self, range:(usize, usize)) -> Result<&[u8], String> {
        if self.empty() {
            return Ok(&[]);
        }

        let max = self.max_index();
        if max.is_err() {
            return Err(format!("? ({:?})", max));
        }
        let max = max.unwrap();

        let from = range.0;
        let to = min(max, range.1);
        if bad_range(&self.all_bytes, (from, to)) {
            return Err(format!("(Bad range: ({}, {}))", range.0, range.1));
        }

        Ok(&self.all_bytes[from..=to])
    }

    /// returns index of the first byte printed on the last line
    /// This is more primitive than `print_bytes`.  It just prints the bytes
    /// from the range.
    pub fn print_bytes_sans_context(&self, range:(usize, usize),
            underline:bool) -> Option<usize> {
        if let Some(range) = self.byte_indices_between((range.0, range.1)) {
            let mut from = range.0;
            let mut last_from = range.0;
            loop {
                if let Some((line, last_byte_index)) = self.line_with_break(
                        from, range.1, false) {
                    println!("{}", line);
                    last_from = from;
                    from = last_byte_index + 1;
                }
                else {
                    break;
                }
            }
            Some(last_from)
        }
        else {
            None
        }
    }


    pub fn empty(&self) -> bool {
        self.all_bytes.len() == 0
    }

    pub fn range(&self) -> (usize, usize) {
        (self.index, self.index + usize::from(self.prefs.width) - 1)
    }

    pub fn max_index(&self) -> Result<usize, String> {
        if self.all_bytes.len() == 0 {
            Err("No bytes, so no max index.".to_owned())
        }
        else {
            Ok(self.all_bytes.len() - 1)
        }
    }
}


impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut to_write:String;
        to_write = "State:\n".to_owned();
        to_write += &format!("  Filename: {}\n", self.filename);
        to_write += &format!("  At byte {} of {}\n", lino(&self),
                hex_unless_dec_with_radix(self.all_bytes.len(), self.prefs.radix));
        if self.unsaved_changes {
            to_write += &format!("  Unwritten changes\n");
        }
        else {
            to_write += &format!("  No unwritten changes\n");
        }
        if self.readonly {
            to_write += &format!("  In read-only mode\n");
        }

        to_write += "  Preferences:\n";

        if self.prefs.show_byte_numbers {
            to_write += &format!("    Printing byte numbers in {}\n",
                string_from_radix(self.prefs.radix));
        };
        if self.prefs.show_chars {
            to_write +=
                    &format!("    Printing char representations after bytes\n");
        };
        to_write += &format!("    Interpreting input numbers as {}\n",
                string_from_radix(self.prefs.radix));
        to_write += &format!("    Printing a newline every {} bytes\n",
                hex_unless_dec_with_radix(usize::from(self.prefs.width),
                        self.prefs.radix));
        if self.prefs.before_context > 0 {
            to_write += &format!("    Printing {} lines before current line\n",
            hex_unless_dec_with_radix(self.prefs.before_context,
                    self.prefs.radix));
        };
        if self.prefs.after_context > 0 {
            to_write += &format!("    Printing {} lines after current line\n",
            hex_unless_dec_with_radix(self.prefs.after_context,
                    self.prefs.radix));
        };
        if self.prefs.color {
            to_write += &format!("    Using {}", self.pretty_color_state());
        }
        else {
            to_write += "    Printing without color";
        }

        write!(f, "{}", to_write)
    }
}


pub fn string_from_bytes(bytes:&[u8]) -> String {
    let mut to_return = "".to_owned();
    for &byte in bytes {
        to_return += &padded_byte(byte);
    }
    return to_return;
}


pub fn padded_byte(byte:u8) -> String {
    format!("{:02x}", byte)
}


pub fn index_of_bytes(needle:&[u8], haystack:&[u8], forward:bool) -> Option<usize> {
    let needle_num_bytes = if needle.len() == 0 {
        return None;
    }
    else {
        needle.len()
    };

    if forward {
        for index in 0..haystack.len() {
            let range = (index, index + needle_num_bytes - 1);

            if bad_range(&haystack.to_vec(), range) {
                return None;
            }

            if &haystack[range.0..=range.1] == needle {
                return Some(index);
            }
        }
    }
    else {
        let max_index = if needle_num_bytes <= haystack.len() {
            haystack.len() - needle_num_bytes
        }
        else {
            0
        };

        for index in (0..=max_index).rev() {
            let range = (index, index + needle_num_bytes - 1);

            if bad_range(&haystack.to_vec(), range) {
                return None;
            }


            let maybe = &haystack[range.0..=range.1];
            if maybe == needle {
                return Some(index);
            }
        }
    }

    None
}


pub fn bytes_from_string(nibbles_s:&str) -> Result<Vec<u8>, String> {
    // TODO Allow general whitespace, not just literal spaces
    let re_bytes = Regex::new(r"^ *([0-9a-fA-F][0-9a-fA-F] *)* *$").unwrap();
    if re_bytes.is_match(&nibbles_s) {
        let nibbles_v:Vec<String> = nibbles_s.replace(" ", "").chars().map(|x| x.to_string()).collect();
        Ok(nibbles_v.chunks(2).map(|x| x.join("")).map(|x| u8::from_str_radix(&x, 16).unwrap()).collect())
    }
    else {
        Err(format!("Couldn't interpret '{}' as a sequence of bytes", &nibbles_s))
    }
}


// TODO: This should take a usize..=usize range object
pub fn bad_range(bytes: &Vec<u8>, range: (usize, usize)) -> bool {
    bytes.len() == 0 || range.1 >= bytes.len()
}


/// Returns new index
pub fn move_to(state:&mut State, index:usize) -> Result<usize, String> {
    if state.empty() {
        Err("Empty file".to_owned())
    }
    else {
        let _max_index = match state.max_index() {
            Ok(max) => max,
            Err(error) => {
                return Err(error);
            },
        };

        if index > _max_index {
            Err(format!("{} > {} = maximum index", hex_unless_dec_with_radix(index, state.prefs.radix), hex_unless_dec_with_radix(_max_index, state.prefs.radix)))
        }
        else {
            state.index = index;
            Ok(index)
        }
    }
}


/// When printing [1, 2, 3, 4, 5, 6, 7] on a width of 3, the
/// maximum 0-up line number is 3.  This function returns that
/// calculation.  See test_max_bytes_line for examples
pub fn max_bytes_line_num(bytes:&[u8], width:NonZeroUsize) -> usize {
    if bytes.len() == 0 {
        0
    }
    else {
        (bytes.len() - 1) / usize::from(width)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bytes_from() {
        let hex_twelve = [
                0x00, 0x01, 0x02,
                0x03, 0x04, 0x05,
                0x06, 0x07, 0x08,
                0x09, 0x0a, 0x0b,
                0x0c, 0x0d, 0x0e,
                0x0f, 0x10, 0x11,
                0x12,
            ];
        let mut state = State {
            prefs: Preferences {
                radix: 16,
                show_byte_numbers: true,
                show_chars: true,
                show_prompt: true,
                color: true,
                width: NonZeroUsize::new(0x10).unwrap(),
                n_padding: "   ".to_owned(),
                after_context: 0,
                before_context: 0,
            },
            unsaved_changes: true,
            index: 0,
            readonly: false,
            last_search: None,
            filename: "filename".to_owned(),
            all_bytes: Vec::from(hex_twelve),
            breaks: HashSet::new(),
        };

        assert_eq!(state.bytes_from(0), &hex_twelve[0x00..=0x0f]);
        assert_eq!(state.bytes_from(1), &hex_twelve[0x01..=0x10]);
        assert_eq!(state.bytes_from(2), &hex_twelve[0x02..=0x11]);
        assert_eq!(state.bytes_from(3), &hex_twelve[0x03..=0x12]);
        assert_eq!(state.bytes_from(4), &hex_twelve[0x04..=0x12]);
        assert_eq!(state.bytes_from(5), &hex_twelve[0x05..=0x12]);
        assert_eq!(state.bytes_from(6), &hex_twelve[0x06..=0x12]);
        assert_eq!(state.bytes_from(7), &hex_twelve[0x07..=0x12]);
        assert_eq!(state.bytes_from(11), &hex_twelve[0x0b..=0x12]);
        assert_eq!(state.bytes_from(17), &hex_twelve[0x11..=0x12]);
        assert_eq!(state.bytes_from(18), &hex_twelve[0x12..=0x12]);
        assert_eq!(state.bytes_from(19), &hex_twelve[0..0]);
        state.prefs.width = NonZeroUsize::new(3).unwrap();
        assert_eq!(state.bytes_from(0), &hex_twelve[0x00..=0x02]);
        assert_eq!(state.bytes_from(1), &hex_twelve[0x01..=0x03]);
        assert_eq!(state.bytes_from(11), &hex_twelve[0x0b..=0x0d]);
        assert_eq!(state.bytes_from(17), &hex_twelve[0x11..=0x12]);
        assert_eq!(state.bytes_from(18), &hex_twelve[0x12..=0x12]);
        assert_eq!(state.bytes_from(19), &hex_twelve[0..0]);
    }

    #[test]
    fn test_line_with_break() {
        let mut states = Vec::new();
        states.push(State {
            prefs: Preferences {
                radix: 16,
                show_byte_numbers: true,
                show_chars: true,
                show_prompt: true,
                color: false,
                after_context: 0,
                before_context: 0,
                width: NonZeroUsize::new(0x10).unwrap(),
                n_padding: "   ".to_owned(),
            },
            unsaved_changes: true,
            readonly: false,
            last_search: None,
            filename: "filename".to_owned(),
            index: 0,
            breaks: HashSet::new(),
            all_bytes: vec![
                0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
                0x10, 0x11, 0x12,
            ],
        });

        assert_eq!(states[0].line_with_break(0x00, 0x00, false), Ok(("    0   |00                                             |   •".to_owned(), 0x00)));
        assert_eq!(states[0].line_with_break(0x00, 0x01, false), Ok(("    0   |00 01                                          |   ••".to_owned(), 0x01)));
        assert_eq!(states[0].line_with_break(0x00, 0x02, false), Ok(("    0   |00 01 02                                       |   •••".to_owned(), 0x02)));
        assert_eq!(states[0].line_with_break(0x00, 0x03, false), Ok(("    0   |00 01 02 03                                    |   ••••".to_owned(), 0x03)));
        assert_eq!(states[0].line_with_break(0x00, 0x04, false), Ok(("    0   |00 01 02 03 04                                 |   •••••".to_owned(), 0x04)));
        assert_eq!(states[0].line_with_break(0x00, 0x05, false), Ok(("    0   |00 01 02 03 04 05                              |   ••••••".to_owned(), 0x05)));
        assert_eq!(states[0].line_with_break(0x00, 0x06, false), Ok(("    0   |00 01 02 03 04 05 06                           |   •••••••".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x00, 0x07, false), Ok(("    0   |00 01 02 03 04 05 06 07                        |   ••••••••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x00, 0x08, false), Ok(("    0   |00 01 02 03 04 05 06 07 08                     |   •••••••••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x00, 0x09, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09                  |   •••••••••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x00, 0x0a, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a               |   •••••••••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x00, 0x0b, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b            |   •••••••••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x00, 0x0c, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c         |   •••••••••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x00, 0x0d, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d      |   •••••••••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x00, 0x0e, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e   |   •••••••••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x00, 0x0f, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f|   •••••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x00, 0x10, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f|   •••••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x00, 0x11, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f|   •••••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x00, 0x12, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f|   •••••••••__•__••".to_owned(), 0x0f)));

        /* too far */
        assert_eq!(states[0].line_with_break(0x00, 0x13, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f|   •••••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x00, 0x14, false), Ok(("    0   |00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f|   •••••••••__•__••".to_owned(), 0x0f)));

        assert_eq!(states[0].line_with_break(0x01, 0x14, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10|   ••••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x01, 0x13, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10|   ••••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x01, 0x12, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10|   ••••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x01, 0x11, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10|   ••••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x01, 0x10, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10|   ••••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x01, 0x0f, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f   |   ••••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x01, 0x0e, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e      |   ••••••••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x01, 0x0d, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c 0d         |   ••••••••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x01, 0x0c, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b 0c            |   ••••••••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x01, 0x0b, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a 0b               |   ••••••••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x01, 0x0a, false), Ok(("    1   |01 02 03 04 05 06 07 08 09 0a                  |   ••••••••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x01, 0x09, false), Ok(("    1   |01 02 03 04 05 06 07 08 09                     |   ••••••••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x01, 0x08, false), Ok(("    1   |01 02 03 04 05 06 07 08                        |   ••••••••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x01, 0x07, false), Ok(("    1   |01 02 03 04 05 06 07                           |   •••••••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x01, 0x06, false), Ok(("    1   |01 02 03 04 05 06                              |   ••••••".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x01, 0x05, false), Ok(("    1   |01 02 03 04 05                                 |   •••••".to_owned(), 0x05)));
        assert_eq!(states[0].line_with_break(0x01, 0x04, false), Ok(("    1   |01 02 03 04                                    |   ••••".to_owned(), 0x04)));
        assert_eq!(states[0].line_with_break(0x01, 0x03, false), Ok(("    1   |01 02 03                                       |   •••".to_owned(), 0x03)));
        assert_eq!(states[0].line_with_break(0x01, 0x02, false), Ok(("    1   |01 02                                          |   ••".to_owned(), 0x02)));
        assert_eq!(states[0].line_with_break(0x01, 0x01, false), Ok(("    1   |01                                             |   •".to_owned(), 0x01)));
        assert_eq!(states[0].line_with_break(0x01, 0x00, false), Err("No bytes in empty interval [1,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x02, 0x14, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11|   •••••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x02, 0x13, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11|   •••••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x02, 0x12, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11|   •••••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x02, 0x11, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11|   •••••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x02, 0x10, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10   |   •••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x02, 0x0f, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f      |   •••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x02, 0x0e, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e         |   •••••••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x02, 0x0d, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c 0d            |   •••••••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x02, 0x0c, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b 0c               |   •••••••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x02, 0x0b, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a 0b                  |   •••••••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x02, 0x0a, false), Ok(("    2   |02 03 04 05 06 07 08 09 0a                     |   •••••••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x02, 0x09, false), Ok(("    2   |02 03 04 05 06 07 08 09                        |   •••••••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x02, 0x08, false), Ok(("    2   |02 03 04 05 06 07 08                           |   •••••••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x02, 0x07, false), Ok(("    2   |02 03 04 05 06 07                              |   ••••••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x02, 0x06, false), Ok(("    2   |02 03 04 05 06                                 |   •••••".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x02, 0x05, false), Ok(("    2   |02 03 04 05                                    |   ••••".to_owned(), 0x05)));
        assert_eq!(states[0].line_with_break(0x02, 0x04, false), Ok(("    2   |02 03 04                                       |   •••".to_owned(), 0x04)));
        assert_eq!(states[0].line_with_break(0x02, 0x03, false), Ok(("    2   |02 03                                          |   ••".to_owned(), 0x03)));
        assert_eq!(states[0].line_with_break(0x02, 0x02, false), Ok(("    2   |02                                             |   •".to_owned(), 0x02)));
        assert_eq!(states[0].line_with_break(0x02, 0x01, false), Err("No bytes in empty interval [2,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x02, 0x00, false), Err("No bytes in empty interval [2,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x03, 0x14, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12|   ••••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x03, 0x13, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12|   ••••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x03, 0x12, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12|   ••••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x03, 0x11, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11   |   ••••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x03, 0x10, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10      |   ••••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x03, 0x0f, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f         |   ••••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x03, 0x0e, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d 0e            |   ••••••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x03, 0x0d, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c 0d               |   ••••••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x03, 0x0c, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b 0c                  |   ••••••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x03, 0x0b, false), Ok(("    3   |03 04 05 06 07 08 09 0a 0b                     |   ••••••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x03, 0x0a, false), Ok(("    3   |03 04 05 06 07 08 09 0a                        |   ••••••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x03, 0x09, false), Ok(("    3   |03 04 05 06 07 08 09                           |   ••••••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x03, 0x08, false), Ok(("    3   |03 04 05 06 07 08                              |   ••••••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x03, 0x07, false), Ok(("    3   |03 04 05 06 07                                 |   •••••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x03, 0x06, false), Ok(("    3   |03 04 05 06                                    |   ••••".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x03, 0x05, false), Ok(("    3   |03 04 05                                       |   •••".to_owned(), 0x05)));
        assert_eq!(states[0].line_with_break(0x03, 0x04, false), Ok(("    3   |03 04                                          |   ••".to_owned(), 0x04)));
        assert_eq!(states[0].line_with_break(0x03, 0x03, false), Ok(("    3   |03                                             |   •".to_owned(), 0x03)));
        assert_eq!(states[0].line_with_break(0x03, 0x02, false), Err("No bytes in empty interval [3,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x03, 0x01, false), Err("No bytes in empty interval [3,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x03, 0x00, false), Err("No bytes in empty interval [3,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x04, 0x14, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12   |   •••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x04, 0x13, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12   |   •••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x04, 0x12, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12   |   •••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x04, 0x11, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11      |   •••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x04, 0x10, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10         |   •••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x04, 0x0f, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e 0f            |   •••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x04, 0x0e, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d 0e               |   •••••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x04, 0x0d, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c 0d                  |   •••••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x04, 0x0c, false), Ok(("    4   |04 05 06 07 08 09 0a 0b 0c                     |   •••••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x04, 0x0b, false), Ok(("    4   |04 05 06 07 08 09 0a 0b                        |   •••••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x04, 0x0a, false), Ok(("    4   |04 05 06 07 08 09 0a                           |   •••••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x04, 0x09, false), Ok(("    4   |04 05 06 07 08 09                              |   •••••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x04, 0x08, false), Ok(("    4   |04 05 06 07 08                                 |   •••••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x04, 0x07, false), Ok(("    4   |04 05 06 07                                    |   ••••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x04, 0x06, false), Ok(("    4   |04 05 06                                       |   •••".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x04, 0x05, false), Ok(("    4   |04 05                                          |   ••".to_owned(), 0x05)));
        assert_eq!(states[0].line_with_break(0x04, 0x04, false), Ok(("    4   |04                                             |   •".to_owned(), 0x04)));
        assert_eq!(states[0].line_with_break(0x04, 0x03, false), Err("No bytes in empty interval [4,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x04, 0x02, false), Err("No bytes in empty interval [4,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x04, 0x01, false), Err("No bytes in empty interval [4,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x04, 0x00, false), Err("No bytes in empty interval [4,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x05, 0x14, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12      |   ••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x05, 0x13, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12      |   ••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x05, 0x12, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12      |   ••••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x05, 0x11, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11         |   ••••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x05, 0x10, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e 0f 10            |   ••••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x05, 0x0f, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e 0f               |   ••••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x05, 0x0e, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d 0e                  |   ••••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x05, 0x0d, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c 0d                     |   ••••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x05, 0x0c, false), Ok(("    5   |05 06 07 08 09 0a 0b 0c                        |   ••••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x05, 0x0b, false), Ok(("    5   |05 06 07 08 09 0a 0b                           |   ••••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x05, 0x0a, false), Ok(("    5   |05 06 07 08 09 0a                              |   ••••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x05, 0x09, false), Ok(("    5   |05 06 07 08 09                                 |   ••••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x05, 0x08, false), Ok(("    5   |05 06 07 08                                    |   ••••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x05, 0x07, false), Ok(("    5   |05 06 07                                       |   •••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x05, 0x06, false), Ok(("    5   |05 06                                          |   ••".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x05, 0x05, false), Ok(("    5   |05                                             |   •".to_owned(), 0x05)));
        assert_eq!(states[0].line_with_break(0x05, 0x04, false), Err("No bytes in empty interval [5,4]".to_owned()));
        assert_eq!(states[0].line_with_break(0x05, 0x03, false), Err("No bytes in empty interval [5,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x05, 0x02, false), Err("No bytes in empty interval [5,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x05, 0x01, false), Err("No bytes in empty interval [5,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x05, 0x00, false), Err("No bytes in empty interval [5,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x06, 0x14, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12         |   •••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x06, 0x13, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12         |   •••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x06, 0x12, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12         |   •••__•__•••••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x06, 0x11, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e 0f 10 11            |   •••__•__••••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x06, 0x10, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e 0f 10               |   •••__•__•••".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x06, 0x0f, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e 0f                  |   •••__•__••".to_owned(), 0x0f)));
        assert_eq!(states[0].line_with_break(0x06, 0x0e, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d 0e                     |   •••__•__•".to_owned(), 0x0e)));
        assert_eq!(states[0].line_with_break(0x06, 0x0d, false), Ok(("    6   |06 07 08 09 0a 0b 0c 0d                        |   •••__•__".to_owned(), 0x0d)));
        assert_eq!(states[0].line_with_break(0x06, 0x0c, false), Ok(("    6   |06 07 08 09 0a 0b 0c                           |   •••__•_".to_owned(), 0x0c)));
        assert_eq!(states[0].line_with_break(0x06, 0x0b, false), Ok(("    6   |06 07 08 09 0a 0b                              |   •••__•".to_owned(), 0x0b)));
        assert_eq!(states[0].line_with_break(0x06, 0x0a, false), Ok(("    6   |06 07 08 09 0a                                 |   •••__".to_owned(), 0x0a)));
        assert_eq!(states[0].line_with_break(0x06, 0x09, false), Ok(("    6   |06 07 08 09                                    |   •••_".to_owned(), 0x09)));
        assert_eq!(states[0].line_with_break(0x06, 0x08, false), Ok(("    6   |06 07 08                                       |   •••".to_owned(), 0x08)));
        assert_eq!(states[0].line_with_break(0x06, 0x07, false), Ok(("    6   |06 07                                          |   ••".to_owned(), 0x07)));
        assert_eq!(states[0].line_with_break(0x06, 0x06, false), Ok(("    6   |06                                             |   •".to_owned(), 0x06)));
        assert_eq!(states[0].line_with_break(0x06, 0x05, false), Err("No bytes in empty interval [6,5]".to_owned()));
        assert_eq!(states[0].line_with_break(0x06, 0x04, false), Err("No bytes in empty interval [6,4]".to_owned()));
        assert_eq!(states[0].line_with_break(0x06, 0x03, false), Err("No bytes in empty interval [6,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x06, 0x02, false), Err("No bytes in empty interval [6,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x06, 0x01, false), Err("No bytes in empty interval [6,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x06, 0x00, false), Err("No bytes in empty interval [6,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x10, 0x14, false), Ok(("   10   |10 11 12                                       |   •••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x10, 0x13, false), Ok(("   10   |10 11 12                                       |   •••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x10, 0x12, false), Ok(("   10   |10 11 12                                       |   •••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x10, 0x11, false), Ok(("   10   |10 11                                          |   ••".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x10, 0x10, false), Ok(("   10   |10                                             |   •".to_owned(), 0x10)));
        assert_eq!(states[0].line_with_break(0x10, 0x0f, false), Err("No bytes in empty interval [16,15]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x0e, false), Err("No bytes in empty interval [16,14]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x0d, false), Err("No bytes in empty interval [16,13]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x0c, false), Err("No bytes in empty interval [16,12]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x0b, false), Err("No bytes in empty interval [16,11]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x0a, false), Err("No bytes in empty interval [16,10]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x09, false), Err("No bytes in empty interval [16,9]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x08, false), Err("No bytes in empty interval [16,8]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x07, false), Err("No bytes in empty interval [16,7]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x06, false), Err("No bytes in empty interval [16,6]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x05, false), Err("No bytes in empty interval [16,5]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x04, false), Err("No bytes in empty interval [16,4]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x03, false), Err("No bytes in empty interval [16,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x02, false), Err("No bytes in empty interval [16,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x01, false), Err("No bytes in empty interval [16,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x10, 0x00, false), Err("No bytes in empty interval [16,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x11, 0x14, false), Ok(("   11   |11 12                                          |   ••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x11, 0x13, false), Ok(("   11   |11 12                                          |   ••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x11, 0x12, false), Ok(("   11   |11 12                                          |   ••".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x11, 0x11, false), Ok(("   11   |11                                             |   •".to_owned(), 0x11)));
        assert_eq!(states[0].line_with_break(0x11, 0x10, false), Err("No bytes in empty interval [17,16]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x0f, false), Err("No bytes in empty interval [17,15]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x0e, false), Err("No bytes in empty interval [17,14]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x0d, false), Err("No bytes in empty interval [17,13]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x0c, false), Err("No bytes in empty interval [17,12]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x0b, false), Err("No bytes in empty interval [17,11]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x0a, false), Err("No bytes in empty interval [17,10]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x09, false), Err("No bytes in empty interval [17,9]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x08, false), Err("No bytes in empty interval [17,8]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x07, false), Err("No bytes in empty interval [17,7]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x06, false), Err("No bytes in empty interval [17,6]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x05, false), Err("No bytes in empty interval [17,5]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x04, false), Err("No bytes in empty interval [17,4]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x03, false), Err("No bytes in empty interval [17,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x02, false), Err("No bytes in empty interval [17,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x01, false), Err("No bytes in empty interval [17,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x11, 0x00, false), Err("No bytes in empty interval [17,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x12, 0x14, false), Ok(("   12   |12                                             |   •".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x12, 0x13, false), Ok(("   12   |12                                             |   •".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x12, 0x12, false), Ok(("   12   |12                                             |   •".to_owned(), 0x12)));
        assert_eq!(states[0].line_with_break(0x12, 0x11, false), Err("No bytes in empty interval [18,17]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x10, false), Err("No bytes in empty interval [18,16]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x0f, false), Err("No bytes in empty interval [18,15]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x0e, false), Err("No bytes in empty interval [18,14]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x0d, false), Err("No bytes in empty interval [18,13]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x0c, false), Err("No bytes in empty interval [18,12]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x0b, false), Err("No bytes in empty interval [18,11]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x0a, false), Err("No bytes in empty interval [18,10]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x09, false), Err("No bytes in empty interval [18,9]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x08, false), Err("No bytes in empty interval [18,8]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x07, false), Err("No bytes in empty interval [18,7]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x06, false), Err("No bytes in empty interval [18,6]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x05, false), Err("No bytes in empty interval [18,5]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x04, false), Err("No bytes in empty interval [18,4]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x03, false), Err("No bytes in empty interval [18,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x02, false), Err("No bytes in empty interval [18,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x01, false), Err("No bytes in empty interval [18,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x12, 0x00, false), Err("No bytes in empty interval [18,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x13, 0x14, false), Err("19 is past the last byte (18)".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x13, false), Err("19 is past the last byte (18)".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x12, false), Err("No bytes in empty interval [19,18]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x11, false), Err("No bytes in empty interval [19,17]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x10, false), Err("No bytes in empty interval [19,16]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x0f, false), Err("No bytes in empty interval [19,15]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x0e, false), Err("No bytes in empty interval [19,14]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x0d, false), Err("No bytes in empty interval [19,13]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x0c, false), Err("No bytes in empty interval [19,12]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x0b, false), Err("No bytes in empty interval [19,11]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x0a, false), Err("No bytes in empty interval [19,10]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x09, false), Err("No bytes in empty interval [19,9]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x08, false), Err("No bytes in empty interval [19,8]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x07, false), Err("No bytes in empty interval [19,7]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x06, false), Err("No bytes in empty interval [19,6]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x05, false), Err("No bytes in empty interval [19,5]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x04, false), Err("No bytes in empty interval [19,4]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x03, false), Err("No bytes in empty interval [19,3]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x02, false), Err("No bytes in empty interval [19,2]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x01, false), Err("No bytes in empty interval [19,1]".to_owned()));
        assert_eq!(states[0].line_with_break(0x13, 0x00, false), Err("No bytes in empty interval [19,0]".to_owned()));

        assert_eq!(states[0].line_with_break(0x14, 0x14, false), Err("20 is past the last byte (18)".to_owned()));
        assert_eq!(states[0].line_with_break(0x14, 0x13, false), Err("No bytes in empty interval [20,19]".to_owned()));

        states.push(State {
            prefs: Preferences {
                radix: 16,
                show_byte_numbers: true,
                show_chars: true,
                show_prompt: true,
                color: false,
                after_context: 0,
                before_context: 0,
                width: NonZeroUsize::new(0x10).unwrap(),
                n_padding: "   ".to_owned(),
            },
            unsaved_changes: true,
            readonly: false,
            last_search: None,
            filename: "filename".to_owned(),
            index: 0,
            breaks: vec![3, 8],
            all_bytes: vec![
                0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
                0x10, 0x11, 0x12,
            ],
        });

        assert_eq!(states[1].line_with_break(0x00, 0x00, false), Ok(("    0   |00                                             |   •".to_owned(), 0x00)));
        assert_eq!(states[1].line_with_break(0x00, 0x01, false), Ok(("    0   |00 01                                          |   ••".to_owned(), 0x01)));
        assert_eq!(states[1].line_with_break(0x00, 0x02, false), Ok(("    0   |00 01 02                                       |   •••".to_owned(), 0x02)));
        assert_eq!(states[1].line_with_break(0x00, 0x03, false), Ok(("    0   |00 01 02                                       |   •••".to_owned(), 0x02)));
        assert_eq!(states[1].line_with_break(0x00, 0x04, false), Ok(("    0   |00 01 02                                       |   •••".to_owned(), 0x02)));
        assert_eq!(states[1].line_with_break(0x00, 0x100, false), Ok(("    0   |00 01 02                                       |   •••".to_owned(), 0x02)));

        assert_eq!(states[1].line_with_break(0x01, 0x0a, false), Ok(("    1   |01 02                                          |   ••".to_owned(), 0x02)));
        assert_eq!(states[1].line_with_break(0x02, 0x0a, false), Ok(("    2   |02                                             |   •".to_owned(), 0x02)));
        assert_eq!(states[1].line_with_break(0x03, 0x0a, false), Ok(("    3   |03 04 05 06 07                                 |   •••••".to_owned(), 0x07)));
        assert_eq!(states[1].line_with_break(0x04, 0x0a, false), Ok(("    4   |04 05 06 07                                    |   ••••".to_owned(), 0x07)));
        assert_eq!(states[1].line_with_break(0x05, 0x0a, false), Ok(("    5   |05 06 07                                       |   •••".to_owned(), 0x07)));
        assert_eq!(states[1].line_with_break(0x06, 0x0a, false), Ok(("    6   |06 07                                          |   ••".to_owned(), 0x07)));
        assert_eq!(states[1].line_with_break(0x07, 0x0a, false), Ok(("    7   |07                                             |   •".to_owned(), 0x07)));
        assert_eq!(states[1].line_with_break(0x08, 0x0a, false), Ok(("    8   |08 09 0a                                       |   •__".to_owned(), 0x0a)));
        assert_eq!(states[1].line_with_break(0x08, 0x100, false), Ok(("    8   |08 09 0a 0b 0c 0d 0e 0f 10 11 12               |   •__•__•••••".to_owned(), 0x12)));
    }


    #[test]
    fn test_byte_numbers() {
        let mut state = State {
            prefs: Preferences {
                radix: 16,
                show_byte_numbers: true,
                show_chars: true,
                show_prompt: true,
                color: true,
                after_context: 0,
                before_context: 0,
                width: NonZeroUsize::new(0x10).unwrap(),
                n_padding: "   ".to_owned(),
            },
            unsaved_changes: true,
            readonly: false,
            last_search: None,
            filename: "filename".to_owned(),
            index: 0,
            breaks: Vec::new(),
            all_bytes: vec![
                0x00, 0x01, 0x02,
                0x03, 0x04, 0x05,
                0x06, 0x07, 0x08,
                0x09, 0x0a, 0x0b,
                0x0c, 0x0d, 0x0e,
                0x0f, 0x10, 0x11,
                0x12,
            ],
        };
        assert_eq!(state.addresses(0x00),
            vec![0x00, 0x10]);
        assert_eq!(state.addresses(0x01),
            vec![0x01, 0x11]);
        assert_eq!(state.addresses(0x04),
            vec![0x04]);
        state.prefs.width = NonZeroUsize::new(3).unwrap();
        assert_eq!(state.addresses(0x04),
            vec![0x04, 0x07, 0x0a, 0x0d, 0x10,]);
    }

    #[test]
    fn test_max_bytes_line() {
        let _1 = NonZeroUsize::new(1).unwrap();
        let _2 = NonZeroUsize::new(2).unwrap();
        let _3 = NonZeroUsize::new(3).unwrap();
        let _4 = NonZeroUsize::new(4).unwrap();
        let _5 = NonZeroUsize::new(5).unwrap();
        let _6 = NonZeroUsize::new(6).unwrap();
        let _7 = NonZeroUsize::new(7).unwrap();
        let _8 = NonZeroUsize::new(8).unwrap();
        let bytes = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13];
        assert_eq!(max_bytes_line_num(&bytes, _1), 12);
        let bytes = vec![8, 6, 7, 5, 3, 0, 9,];
        assert_eq!(max_bytes_line_num(&bytes, _1), 6);
        assert_eq!(max_bytes_line_num(&bytes, _2), 3);
        assert_eq!(max_bytes_line_num(&bytes, _3), 2);
        assert_eq!(max_bytes_line_num(&bytes, _4), 1);
        assert_eq!(max_bytes_line_num(&bytes, _5), 1);
        assert_eq!(max_bytes_line_num(&bytes, _6), 1);
        assert_eq!(max_bytes_line_num(&bytes, _7), 0);
        assert_eq!(max_bytes_line_num(&bytes, _8), 0);
        let bytes = vec![8, 6, 7, 5, 3, 0,];
        assert_eq!(max_bytes_line_num(&bytes, _1), 5);
        assert_eq!(max_bytes_line_num(&bytes, _2), 2);
        assert_eq!(max_bytes_line_num(&bytes, _3), 1);
        assert_eq!(max_bytes_line_num(&bytes, _4), 1);
        assert_eq!(max_bytes_line_num(&bytes, _5), 1);
        assert_eq!(max_bytes_line_num(&bytes, _6), 0);
        assert_eq!(max_bytes_line_num(&bytes, _7), 0);
        assert_eq!(max_bytes_line_num(&bytes, _8), 0);
        let bytes = vec![8, 6, 7,];
        assert_eq!(max_bytes_line_num(&bytes, _1), 2);
        assert_eq!(max_bytes_line_num(&bytes, _2), 1);
        assert_eq!(max_bytes_line_num(&bytes, _3), 0);
        assert_eq!(max_bytes_line_num(&bytes, _4), 0);
        assert_eq!(max_bytes_line_num(&bytes, _5), 0);
        let bytes = vec![8, 6,];
        assert_eq!(max_bytes_line_num(&bytes, _1), 1);
        assert_eq!(max_bytes_line_num(&bytes, _2), 0);
        assert_eq!(max_bytes_line_num(&bytes, _3), 0);
        assert_eq!(max_bytes_line_num(&bytes, _4), 0);
        let bytes = vec![8,];
        assert_eq!(max_bytes_line_num(&bytes, _1), 0);
        assert_eq!(max_bytes_line_num(&bytes, _2), 0);
        assert_eq!(max_bytes_line_num(&bytes, _3), 0);
        assert_eq!(max_bytes_line_num(&bytes, _4), 0);
        let bytes = vec![];
        assert_eq!(max_bytes_line_num(&bytes, _1), 0);
        assert_eq!(max_bytes_line_num(&bytes, _2), 0);
        assert_eq!(max_bytes_line_num(&bytes, _3), 0);
        assert_eq!(max_bytes_line_num(&bytes, _4), 0);
    }



    #[test]
    fn test_padded_byte() {
        assert_eq!(padded_byte(2), "02");
        assert_eq!(padded_byte(10), "0a");
    }

    #[test]
    fn test_index_of_bytes() {
        let haystack = vec![0xde, 0xad, 0xbe, 0xef];
        assert_eq!(index_of_bytes(&vec![], &haystack, true), None);
        assert_eq!(index_of_bytes(&vec![0xad, 0xbe], &haystack, true), Some(1));
        assert_eq!(index_of_bytes(&vec![0xad, 0xbe, 0xef], &haystack, true), Some(1));
        assert_eq!(index_of_bytes(&vec![0xad, 0xbe, 0xef, 0xef], &haystack, true), None);
        assert_eq!(index_of_bytes(&vec![0xde,], &haystack, true), Some(0));
    }


    #[test]
    fn test_num_graphemes() {
        assert_eq!(num_graphemes("hey, there"), 10);
        assert_eq!(num_graphemes("दीपक"), 3);
        assert_eq!(num_graphemes("ﷺ"), 1);
        assert_eq!(num_graphemes("père"), 4);
    }


    #[test]
    fn test_bytes_line() {
        let bytes = vec![];
        let _1 = NonZeroUsize::new(1).unwrap();
        let _2 = NonZeroUsize::new(2).unwrap();
        let _3 = NonZeroUsize::new(3).unwrap();
        let _4 = NonZeroUsize::new(4).unwrap();
        let _5 = NonZeroUsize::new(5).unwrap();
        let _6 = NonZeroUsize::new(6).unwrap();
        let _7 = NonZeroUsize::new(7).unwrap();
        let _8 = NonZeroUsize::new(8).unwrap();
        let _empty: Vec<u8> = vec![];
        assert_eq!(bytes_line_bytes(&bytes, 0, _1).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _2).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 1, _1).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 1, _2).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 2, _1).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 2, _2).to_owned(), _empty);
        let bytes = vec![8, 6, 7, 5, 3, 0, 9,];
        assert_eq!(bytes_line_bytes(&bytes, 0, _1).to_owned(), vec![8,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _1).to_owned(), vec![6,]);
        assert_eq!(bytes_line_bytes(&bytes, 2, _1).to_owned(), vec![7,]);
        assert_eq!(bytes_line_bytes(&bytes, 3, _1).to_owned(), vec![5,]);
        assert_eq!(bytes_line_bytes(&bytes, 4, _1).to_owned(), vec![3,]);
        assert_eq!(bytes_line_bytes(&bytes, 5, _1).to_owned(), vec![0,]);
        assert_eq!(bytes_line_bytes(&bytes, 6, _1).to_owned(), vec![9,]);
        assert_eq!(bytes_line_bytes(&bytes, 7, _1).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 8, _1).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 9, _1).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _2).to_owned(), vec![8, 6,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _2).to_owned(), vec![7, 5,]);
        assert_eq!(bytes_line_bytes(&bytes, 2, _2).to_owned(), vec![3, 0,]);
        assert_eq!(bytes_line_bytes(&bytes, 3, _2).to_owned(), vec![9,]);
        assert_eq!(bytes_line_bytes(&bytes, 4, _2).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 5, _2).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _3).to_owned(), vec![8, 6, 7,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _3).to_owned(), vec![5, 3, 0,]);
        assert_eq!(bytes_line_bytes(&bytes, 2, _3).to_owned(), vec![9,]);
        assert_eq!(bytes_line_bytes(&bytes, 3, _3).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 4, _3).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _4).to_owned(), vec![8, 6, 7, 5,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _4).to_owned(), vec![3, 0, 9,]);
        assert_eq!(bytes_line_bytes(&bytes, 2, _4).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 3, _4).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 4, _4).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _5).to_owned(), vec![8, 6, 7, 5, 3,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _5).to_owned(), vec![0, 9,]);
        assert_eq!(bytes_line_bytes(&bytes, 2, _5).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 3, _5).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _6).to_owned(), vec![8, 6, 7, 5, 3, 0,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _6).to_owned(), vec![9,]);
        assert_eq!(bytes_line_bytes(&bytes, 2, _6).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 3, _6).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _7).to_owned(), vec![8, 6, 7, 5, 3, 0, 9,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _7).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 2, _7).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 3, _7).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 0, _8).to_owned(), vec![8, 6, 7, 5, 3, 0, 9,]);
        assert_eq!(bytes_line_bytes(&bytes, 1, _8).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 2, _8).to_owned(), _empty);
        assert_eq!(bytes_line_bytes(&bytes, 3, _8).to_owned(), _empty);
    }
}


pub fn bytes_line_range(bytes:&[u8], line_number:usize, width:NonZeroUsize) -> std::ops::Range<usize> {
    let width = usize::from(width);
    if line_number * width < bytes.len() {
        let end_index = min(bytes.len(), line_number * width + width);
        line_number * width..end_index
    }
    else {
        0..0
    }
}


pub fn bytes_line_bytes(bytes:&[u8], line_number:usize, width:NonZeroUsize) -> &[u8] {
    &bytes[bytes_line_range(bytes, line_number, width)]
}


pub fn chars_line_chars(bytes:&[u8], line_number:usize, width:NonZeroUsize) -> Vec<char> {
    let mut to_return:Vec<char> = vec![];
    for index in bytes_line_range(bytes, line_number, width) {
        to_return.push(chared_byte(bytes[index]));
    }

    to_return
}


pub fn colored_chared_bytes(bytes:&[u8], line_number:usize, width:NonZeroUsize,
        underline:bool) -> Vec<String> {
    let mut to_return:Vec<String> = vec![];
    for index in bytes_line_range(bytes, line_number, width) {
            to_return.push(String::from(colored_chared_byte(bytes[index], underline)));
    }

    to_return
}


pub fn chared_bytes(bytes:&[u8], line_number:usize, width:NonZeroUsize) -> Vec<char> {
    let mut to_return:Vec<char> = vec![];
    for index in bytes_line_range(bytes, line_number, width) {
        to_return.push(chared_byte(bytes[index]));
    }

    to_return
}


pub fn chars_line(bytes:&[u8], line_number:usize, width:NonZeroUsize,
        color:bool, underline:bool) -> String {
    let mut to_return:String = "".to_owned();
    if color {
        for colored_char in colored_chared_bytes(bytes, line_number, width,
                underline) {
            to_return += &colored_char;
        }
    }
    else {
        for chared in chared_bytes(bytes, line_number, width) {
            to_return += &chared.to_string();
        }
    }

    to_return
}


fn formatted_byte(byte:u8, color:bool, underline:bool) -> String {
    if color {
        if underline {
            Byte(byte).color().underline().paint(padded_byte(byte)).to_string()
        }
        else {
            Byte(byte).color().paint(padded_byte(byte)).to_string()
        }
    }
    else {
        padded_byte(byte)
    }
}


fn formatted_char(byte:u8, color:bool, underline:bool) -> String {
  if color {
    colored_chared_byte(byte, underline)
  }
  else {
    chared_byte(byte).to_string()
  }
}


fn colored_chared_byte(byte:u8, underline: bool) -> String {
    if underline {
        Byte(byte).color().underline().paint(String::from(Byte(byte).as_char())).to_string()
    }
    else {
        Byte(byte).color().paint(String::from(Byte(byte).as_char())).to_string()
    }
}


pub fn chared_byte(byte:u8) -> char {
    Byte(byte).as_char()
}


/// .len gives the number of bytes
/// .chars.count() gives the number of characters (which counts è as two characters.
/// The human concept is unicode "graphemes" or "glyphs" defined to be what
/// think they are.
pub fn num_graphemes(unicode_string: &str) -> usize {
    return unicode_string.graphemes(true).count();
}


pub fn cargo_version() -> Result<String, String> {
    if let Some(version) = option_env!("CARGO_PKG_VERSION") {
        return Ok(String::from(version));
    }
    return Err("Version unknown (not compiled with cargo)".to_string());
}


pub fn address_display(address: usize, radix:u32, padding:&str,
        underline:bool) -> String {
    let address = if radix == 10 {
        format!("{:>5}", address)
    }
    else {
        format!("{:>5x}", address)
    };

    let address = if underline {
        format!("{}", Style::new().underline().paint(address))
    }
    else {
        format!("{}", address)
    };

    format!("{}{}", address, padding)
}


pub fn filehandle(filename:&str) -> Result<Option<File>, String> {
    match File::open(filename) {
        Ok(filehandle) => {
            Ok(Some(filehandle))
        },
        Err(error) => {
            if error.kind() == std::io::ErrorKind::NotFound {
                Ok(None)
            }
            else {
                Err(format!("Error opening '{}'", filename))
            }
        },
    }
}


pub fn get_input_or_die() -> Result<String, i32> {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(_num_bytes) => {

            /* EOF Return error of 0 to indicate time for a clean exit.  */
            if _num_bytes == 0 {
                Err(0)
            }
            else {
                Ok(input.trim().to_string())
            }
        }
        Err(_) => {
            println!("Unable to read input");
            Err(3)
        }
    }
}


pub fn read_string_from_user(prompt: Option<&str>) -> Result<String, String> {
    print!("{}", if prompt.is_none() {
            "> "
        }
        else {
            prompt.unwrap()
        }
    );

    io::stdout().flush().unwrap();
    let result = get_input_or_die();

    if result.is_ok() {
        Ok(result.unwrap())
    }

    /* Consider EOF empty string */
    else if result == Err(0) {
        Ok("".to_owned())
    }

    else {
        Err("Couldn't read input from user".to_owned())
    }
}


pub fn save_to_path_or_default<T: DiskWritable>(to_be_saved: T, prompt: &str,
        default_path: PathBuf) {
    let filename = read_string_from_user(Some(&format!(
            "{} [{}]: ", prompt, default_path.display())));

    if filename.is_ok() {
        let mut filename = filename.unwrap();
        if filename == "" {
            if let Some(default_path_s) = default_path.to_str() {
                filename = default_path_s.to_owned();

                /* In the default case, we're brave enough to create
                 * the parent directory for the file if it's not root */
                if let Some(parent_dir) = default_path.parent() {
                    if let Err(error) = fs::create_dir_all(parent_dir) {
                        println!("? Couldn't create director {} ({:?})",
                                parent_dir.display(), error);
                    }
                }
            }
            else {
                println!("? Default path ({}) is not valid unicode.",
                        default_path.display());
                return;
            }
        }

        let result = to_be_saved.write_to_disk(&filename);
        if let Err(error) = result {
            println!("? {:?}", error);
        }
    }
    else {
        println!("? {:?}", filename);
    }
}
