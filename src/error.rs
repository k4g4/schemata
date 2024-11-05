use nom::{
    error::{ContextError, ErrorKind, FromExternalError, ParseError},
    AsBytes,
};
use std::{error, fmt, str};

use crate::globals;

#[derive(Debug)]
pub struct ParserError<I> {
    input: I,
    kind: ParserErrorKind<I>,
}

#[derive(Debug)]
pub enum ParserErrorKind<I> {
    Parse {
        kind: ErrorKind,
    },
    Expected {
        char: char,
    },
    Appended {
        kind: ErrorKind,
        error: Box<ParserError<I>>,
    },
    WithContext {
        ctx: String,
        error: Box<ParserError<I>>,
    },
    External {
        kind: ErrorKind,
        external: Box<dyn error::Error + Send + Sync>,
    },
}

impl<I: AsBytes> fmt::Display for ParserError<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Ok(input) = str::from_utf8(self.input.as_bytes()) {
            let preview: String = input
                .lines()
                .take(if globals::debug() { 10 } else { 3 })
                .map(|line| "> ".to_string() + line + "\n")
                .collect();
            writeln!(f, "\n{preview}")?;
        }
        writeln!(f, "{}", self.kind)
    }
}

impl<I: AsBytes> fmt::Display for ParserErrorKind<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parse { kind } => {
                write!(f, "[Inside {}]", kind.description())
            }
            Self::Expected { char } => write!(f, "[Expected '{char}']",),
            Self::Appended { kind, error } => {
                write!(f, "[Inside {}]\n{error}", kind.description())
            }
            Self::WithContext { ctx, error } => write!(f, "[Failed during '{ctx}']\n{error}"),
            Self::External { kind, external } => {
                write!(f, "[Inside {}]\n[{external}]", kind.description())
            }
        }
    }
}

impl<I: AsBytes + fmt::Debug> error::Error for ParserError<I> {}

impl<I> ParseError<I> for ParserError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            input,
            kind: ParserErrorKind::Parse { kind },
        }
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        Self {
            kind: ParserErrorKind::Appended {
                kind,
                error: Box::new(other),
            },
            input,
        }
    }

    fn from_char(input: I, char: char) -> Self {
        Self {
            input,
            kind: ParserErrorKind::Expected { char },
        }
    }
}

impl<I> ContextError<I> for ParserError<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        Self {
            kind: ParserErrorKind::WithContext {
                ctx: ctx.into(),
                error: Box::new(other),
            },
            input,
        }
    }
}

impl<I, E: error::Error + Send + Sync + 'static> FromExternalError<I, E> for ParserError<I> {
    fn from_external_error(input: I, kind: ErrorKind, external: E) -> Self {
        Self {
            kind: ParserErrorKind::External {
                kind,
                external: Box::new(external),
            },
            input,
        }
    }
}
