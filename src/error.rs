use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};
use std::{
    backtrace::{Backtrace, BacktraceStatus},
    error, fmt,
};

#[derive(Debug)]
pub enum ParserError {
    Parse {
        bt: Backtrace,
        kind: ErrorKind,
    },
    Expected {
        char: char,
    },
    Appended {
        kind: ErrorKind,
        error: Box<ParserError>,
    },
    WithContext {
        ctx: String,
        error: Box<ParserError>,
    },
    External {
        kind: ErrorKind,
        external: Box<dyn error::Error + Send + Sync>,
    },
    Unexpected,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parse { bt, .. } if bt.status() == BacktraceStatus::Captured => {
                write!(f, "{bt}")
            }
            Self::Parse { kind, .. } => write!(f, "[Inside {}]", kind.description()),
            Self::Expected { char } => write!(f, "[Expected '{char}']"),
            Self::Appended { kind, error } => {
                write!(f, "{error}\n[Inside {}]", kind.description())
            }
            Self::WithContext { ctx, error } => write!(f, "[Failed during '{ctx}']\n{error}"),
            Self::External { kind, external } => {
                write!(f, "[{external}]\n[Inside {}]", kind.description())
            }
            Self::Unexpected => write!(f, "[Unexpected error]"),
        }
    }
}

impl error::Error for ParserError {}

impl<I> ParseError<I> for ParserError {
    fn from_error_kind(_: I, kind: ErrorKind) -> Self {
        Self::Parse {
            bt: Backtrace::capture(),
            kind,
        }
    }

    fn append(_: I, kind: ErrorKind, other: Self) -> Self {
        Self::Appended {
            kind,
            error: Box::new(other),
        }
    }

    fn from_char(_: I, char: char) -> Self {
        Self::Expected { char }
    }
}

impl<I> ContextError<I> for ParserError {
    fn add_context(_: I, ctx: &'static str, other: Self) -> Self {
        Self::WithContext {
            ctx: ctx.into(),
            error: Box::new(other),
        }
    }
}

impl<I, E: error::Error + Send + Sync + 'static> FromExternalError<I, E> for ParserError {
    fn from_external_error(_: I, kind: ErrorKind, e: E) -> Self {
        Self::External {
            kind,
            external: Box::new(e),
        }
    }
}
