use std::cell::Cell;

thread_local! {
    static DEBUG: Cell<bool> = Default::default();
    static PRETTY: Cell<bool> = Default::default();
    static GC_FREQ: Cell<usize> = Default::default();
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

pub fn gc_freq() -> usize {
    GC_FREQ.get()
}

pub fn set_gc_freq(freq: usize) {
    GC_FREQ.set(freq)
}
