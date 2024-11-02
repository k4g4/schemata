use std::cell::Cell;

thread_local! {
    static DEBUG: Cell<bool> = Default::default();
}

pub fn debug() -> bool {
    DEBUG.get()
}

pub fn set_debug(debug: bool) {
    DEBUG.set(debug)
}
