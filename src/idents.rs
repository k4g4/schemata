macro_rules! idents {
    ($($ident:ident: $def:literal),* $(,)?) => {
        $(pub const $ident: &str = $def;)*
    };
}

idents! {
    DEFINE: "define",
    LAMBDA: "lambda",
    LET:    "let",
    CONS:   "cons",
    CAR:    "car",
    CDR:    "cdr",
    COND:   "cond",
    ELSE:   "else",
    IF:     "if",
    AND:    "and",
    OR:     "or",

    DISP:   "display",
    NEWL:   "newline",
    ERROR:  "error",

    ADD:    "+",
    SUB:    "-",
    MUL:    "*",
    DIV:    "/",

    LOG:    "log",
    EXP:    "exp",
    REM:    "remainder",
    TRUNC:  "truncate",
    FLOOR:  "floor",
    CEIL:   "ceiling",

    SIN:    "sin",
    COS:    "cos",
    TAN:    "tan",
    ASIN:   "asin",
    ACOS:   "acos",
    ATAN:   "atan",
    SINH:   "sinh",
    COSH:   "cosh",
    TANH:   "tanh",
    ASINH:  "asinh",
    ACOSH:  "acosh",
    ATANH:  "atanh",

    TRUE:   "#t",
    FALSE:  "#f",

    EQ:     "=",
    GT:     ">",
    GE:     ">=",
    LT:     "<",
    LE:     "<=",
}
