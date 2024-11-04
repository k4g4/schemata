use std::cell::Cell;

thread_local! {
    static DEBUG: Cell<bool> = Default::default();
    static PRETTY: Cell<bool> = Default::default();
}

pub fn debug() -> bool {
    DEBUG.get()
}

pub fn set_debug(debug: bool) {
    DEBUG.set(debug)
}

pub fn pretty() -> bool {
    PRETTY.get()
}

pub fn set_pretty(pretty: bool) {
    PRETTY.set(pretty)
}
