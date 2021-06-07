use ansi_term::Color::Fixed;
use ansi_term::Color;
use std::fmt;
use std::num::NonZeroUsize;

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


pub struct State {
    pub radix: u32,
    pub show_byte_numbers: bool,
    pub show_chars: bool,
    pub unsaved_changes: bool,
    pub filename: String,

    pub show_prompt: bool,
    pub color: bool,

    /* Current byte number, 0 to len -1 */
    pub index: usize,

    pub width: NonZeroUsize,

    /* The bytes in memory */
    pub all_bytes: Vec<u8>,

    /* Spaces to put between a byte number and a byte when displaying */
    pub n_padding: String,
}

pub fn lino(state:&State) -> String {
    hex_unless_dec_with_radix(state.index, state.radix)
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
        write!(f, "radix: {}|unsaved_changes: {}|show_byte_numbers: {}|show_chars: {}|index: {}|width: {}|n_padding: '{}'|filename: {}|",
                self.radix, self.unsaved_changes, self.show_byte_numbers,
                self.show_chars, self.index, self.width, self.n_padding,
                self.filename)
    }
}


impl State {
    pub fn empty(&self) -> bool {
        self.all_bytes.len() == 0
    }

    pub fn range(&self) -> (usize, usize) {
        (self.index, self.index + usize::from(self.width) - 1)
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
        write!(f, "At byte {} of {}\n", lino(&self),
                hex_unless_dec_with_radix(self.all_bytes.len(), self.radix));
        if self.show_byte_numbers {
            write!(f, "Printing byte numbers in {}\n",
                string_from_radix(self.radix));
        };
        if self.show_chars {
            write!(f, "Printing char representations after bytes\n");
        };
        write!(f, "Interpreting input numbers as {}\n",
                string_from_radix(self.radix));
        write!(f, "Printing a newline every {} bytes\n",
                hex_unless_dec_with_radix(usize::from(self.width), self.radix));
        if self.unsaved_changes {
            write!(f, "Unwritten changes")
        }
        else {
            write!(f, "No unwritten changes")
        }
    }
}
