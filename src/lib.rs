use ansi_term::Color::Fixed;
use ansi_term::Color;

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
