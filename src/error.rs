use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};
use std::{
    backtrace::{Backtrace, BacktraceStatus},
    error, fmt,
};

#[derive(Debug)]
pub enum Error {
    Parse {
        bt: Backtrace,
        kind: ErrorKind,
    },
    Expected {
        char: char,
    },
    Appended {
        kind: ErrorKind,
        error: Box<Error>,
    },
    WithContext {
        ctx: String,
        error: Box<Error>,
    },
    External {
        kind: ErrorKind,
        external: Box<dyn error::Error>,
    },
    Unexpected,
    Other(String),
}

impl fmt::Display for Error {
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
            Self::Other(description) => write!(f, "[{description}]"),
        }
    }
}

impl error::Error for Error {}

impl<I> ParseError<I> for Error {
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

impl<I> ContextError<I> for Error {
    fn add_context(_: I, ctx: &'static str, other: Self) -> Self {
        Self::WithContext {
            ctx: ctx.into(),
            error: Box::new(other),
        }
    }
}

impl<I, E: error::Error + 'static> FromExternalError<I, E> for Error {
    fn from_external_error(_: I, kind: ErrorKind, e: E) -> Self {
        Self::External {
            kind,
            external: Box::new(e),
        }
    }
}
