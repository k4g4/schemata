use std::cell::Cell;

thread_local! {
    static DEBUG: Cell<bool> = Default::default();
    static PRETTY: Cell<bool> = Default::default();
    static GC_FREQ: Cell<u64> = Default::default();
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

pub fn gc_freq() -> u64 {
    GC_FREQ.get()
}

pub fn set_gc_freq(freq: u64) {
    GC_FREQ.set(freq)
}
