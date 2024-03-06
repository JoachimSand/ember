pub const RED_START: &str = "\x1b[1;31m";
pub const B_RED_START: &str = "\x1b[1;91m";
pub const B_BLUE_START: &str = "\x1b[1;94m";
pub const B_MAGENTA_START: &str = "\x1b[1;95m";
pub const FORMAT_END: &str = "\x1b[0m";

// TODO: This approach can be improved greatly.
#[macro_export]
macro_rules! black {
    ( $l:literal) => {{
        colour!($l, "1;30m")
    }};
}

#[macro_export]
macro_rules! red {
    ( $l:literal) => {{
        colour!($l, "1;31m")
    }};
}

#[macro_export]
macro_rules! green {
    ( $l:literal) => {{
        colour!($l, "1;32m")
    }};
}

#[macro_export]
macro_rules! yellow {
    ( $l:literal) => {{
        colour!($l, "1;33m")
    }};
}

#[macro_export]
macro_rules! blue {
    ( $l:literal) => {{
        colour!($l, "1;34m")
    }};
}

#[macro_export]
macro_rules! magenta {
    ( $l:literal) => {{
        colour!($l, "1;35m")
    }};
}

#[macro_export]
macro_rules! cyan {
    ( $l:literal) => {{
        colour!($l, "1;36m")
    }};
}

#[macro_export]
macro_rules! white {
    ( $l:literal) => {{
        colour!($l, "1;37m")
    }};
}

#[macro_export]
macro_rules! default {
    ( $l:literal) => {{
        colour!($l, "1;39m")
    }};
}

#[macro_export]
macro_rules! b_black {
    ( $l:literal) => {{
        colour!($l, "1;90m")
    }};
}

#[macro_export]
macro_rules! b_red {
    ( $l:literal) => {{
        colour!($l, "1;91m")
    }};
}

#[macro_export]
macro_rules! b_green {
    ( $l:literal) => {{
        colour!($l, "1;92m")
    }};
}

#[macro_export]
macro_rules! b_yellow {
    ( $l:literal) => {{
        colour!($l, "1;93m")
    }};
}

#[macro_export]
macro_rules! b_blue {
    ( $l:literal) => {{
        colour!($l, "1;94m")
    }};
}

#[macro_export]
macro_rules! b_magenta {
    ( $l:literal) => {{
        colour!($l, "1;95m")
    }};
}

#[macro_export]
macro_rules! b_cyan {
    ( $l:literal) => {{
        colour!($l, "1;96m")
    }};
}

#[macro_export]
macro_rules! d_white {
    ( $l:literal) => {{
        colour!($l, "1;37;1m")
    }};
}

#[macro_export]
macro_rules! colour {
    ( $l:expr, $format:literal) => {{
        concat!("\x1b[", $format, $l, "\x1b[0m")
    }};
}

#[allow(unused_imports)]
pub(crate) use {
    b_black, b_blue, b_cyan, b_green, b_magenta, b_red, b_yellow, black, blue, colour, cyan,
    d_white, default, green, magenta, red, white, yellow,
};
