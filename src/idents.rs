macro_rules! idents {
    ($($ident:ident: $def:literal),* $(,)?) => {
        $(pub const $ident: &str = $def;)*
    };
}

idents! {
    DEFINE:     "define",
    LAMBDA:     "lambda",
    LET:        "let",
    COND:       "cond",
    ELSE:       "else",
    IF:         "if",
    AND:        "and",
    OR:         "or",

    APPLY:      "apply",

    CONS:       "cons",
    LIST:       "list",
    FIRST:      "first",
    SECOND:     "second",
    THIRD:      "third",
    FOURTH:     "fourth",
    FIFTH:      "fifth",
    SIXTH:      "sixth",
    SEVENTH:    "seventh",
    EIGHTH:     "eighth",
    NINTH:      "ninth",
    TENTH:      "tenth",

    DISP:       "display",
    NEWL:       "newline",
    ERROR:      "error",

    ADD:        "+",
    SUB:        "-",
    MUL:        "*",
    DIV:        "/",

    LOG:        "log",
    EXP:        "exp",
    REM:        "remainder",
    TRUNC:      "truncate",
    FLOOR:      "floor",
    CEIL:       "ceiling",

    SIN:        "sin",
    COS:        "cos",
    TAN:        "tan",
    ASIN:       "asin",
    ACOS:       "acos",
    ATAN:       "atan",
    SINH:       "sinh",
    COSH:       "cosh",
    TANH:       "tanh",
    ASINH:      "asinh",
    ACOSH:      "acosh",
    ATANH:      "atanh",

    TRUE:       "#t",
    FALSE:      "#f",
    VOID:       "#!void",

    EQ:         "=",
    GT:         ">",
    GE:         ">=",
    LT:         "<",
    LE:         "<=",
}
