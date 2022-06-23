use std::{fmt, mem, ops, ptr, str};

#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct AsciiChar {
    ch: u8,
}

impl AsciiChar {
    #[inline]
    pub fn new(ch: char) -> Self {
        assert!(
            ch.is_ascii(),
            "Tried to create `AsciiChar` from non ASCII `char`"
        );
        Self { ch: ch as _ }
    }

    #[inline]
    pub fn new_u8(ch: u8) -> Self {
        assert!(
            ch.is_ascii(),
            "Tried to create `AsciiChar` from non ASCII `u8`"
        );
        Self { ch }
    }

    #[inline]
    pub const unsafe fn new_unchecked(ch: char) -> Self {
        Self { ch: ch as _ }
    }

    #[inline]
    pub const unsafe fn new_u8_unchecked(ch: u8) -> Self {
        Self { ch }
    }

    #[inline]
    pub fn as_char(self) -> char {
        unsafe { char::from_u32_unchecked(self.ch as _) }
    }

    /// Always returns true. This function exists for the sake of optimizing basic `.is_ascii()`
    /// calls after replacing a `u8` with `AsciiChar`.
    #[inline]
    pub const fn is_ascii(&self) -> bool {
        true
    }
}

impl ops::Deref for AsciiChar {
    type Target = u8;

    #[inline]
    fn deref(&self) -> &u8 {
        &self.ch
    }
}

impl Into<char> for AsciiChar {
    #[inline]
    fn into(self) -> char {
        self.as_char()
    }
}

impl TryFrom<u8> for AsciiChar {
    type Error = ();

    fn try_from(ch: u8) -> Result<Self, Self::Error> {
        if ch.is_ascii() {
            Ok(Self { ch })
        } else {
            Err(())
        }
    }
}

impl TryFrom<char> for AsciiChar {
    type Error = ();

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        if ch.is_ascii() {
            Ok(Self { ch: ch as _ })
        } else {
            Err(())
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(C, align(8))]
pub struct Ascii7 {
    len: u8,
    chars: [u8; 7],
}

impl Ascii7 {
    #[inline]
    pub fn new() -> Self {
        Self {
            len: 0,
            chars: [0; 7],
        }
    }

    #[inline]
    pub fn len(self) -> usize {
        self.len as _
    }

    pub fn push(&mut self, ch: AsciiChar) -> bool {
        if self.len() < self.chars.len() {
            self.chars[self.len()] = *ch;
            self.len += 1;
            true
        } else {
            false
        }
    }

    pub fn pop(&mut self) -> Option<u8> {
        unsafe {
            if self.len == 0 {
                None
            } else {
                self.len -= 1;
                Some(mem::replace(
                    self.chars.get_unchecked_mut(self.len as usize),
                    0,
                ))
            }
        }
    }
}

#[derive(Clone, Copy, Hash)]
pub enum Ascii7ConversionError {
    TooLong,
    NotAscii,
}

impl fmt::Display for Ascii7ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooLong => write!(
                f,
                "Tried to convert a string longer than 7 characters to `Ascii7`"
            ),
            Self::NotAscii => write!(f, "Tried to convert a non ASCII string to `Ascii7`"),
        }
    }
}

impl fmt::Debug for Ascii7ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", std::any::type_name::<Self>())?;
        match self {
            Self::TooLong => write!(f, "TooLong")?,
            Self::NotAscii => write!(f, "NotAscii")?,
        }
        write!(f, " (\"{}\")", self)?;
        Ok(())
    }
}

impl std::error::Error for Ascii7ConversionError {}

impl<'a> TryFrom<&'a [AsciiChar]> for Ascii7 {
    type Error = Ascii7ConversionError;

    fn try_from(s: &'a [AsciiChar]) -> Result<Self, Self::Error> {
        if 7 < s.len() {
            Err(Ascii7ConversionError::TooLong)
        } else {
            let mut slf = Self::new();
            unsafe {
                ptr::copy_nonoverlapping(s.as_ptr() as *const u8, slf.chars.as_mut_ptr(), s.len());
            }
            slf.len = s.len() as _;
            Ok(slf)
        }
    }
}

impl<'a> TryFrom<&'a [u8]> for Ascii7 {
    type Error = Ascii7ConversionError;

    fn try_from(s: &'a [u8]) -> Result<Self, Self::Error> {
        if 7 < s.len() {
            Err(Ascii7ConversionError::TooLong)
        } else if !s.is_ascii() {
            Err(Ascii7ConversionError::NotAscii)
        } else {
            let mut slf = Self::new();
            unsafe {
                ptr::copy_nonoverlapping(s.as_ptr(), slf.chars.as_mut_ptr(), s.len());
            }
            slf.len = s.len() as _;
            Ok(slf)
        }
    }
}

impl<'a> TryFrom<&'a str> for Ascii7 {
    type Error = Ascii7ConversionError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        if 7 < s.len() {
            Err(Ascii7ConversionError::TooLong)
        } else if !s.is_ascii() {
            Err(Ascii7ConversionError::NotAscii)
        } else {
            let mut slf = Self::new();
            unsafe {
                ptr::copy_nonoverlapping(s.as_ptr(), slf.chars.as_mut_ptr(), s.len());
            }
            slf.len = s.len() as _;
            Ok(slf)
        }
    }
}

impl ops::Deref for Ascii7 {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        unsafe { str::from_utf8_unchecked(&self.chars.get_unchecked(..self.len as usize)) }
    }
}

impl fmt::Display for Ascii7 {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Debug for Ascii7 {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Debug>::fmt(self, f)
    }
}
