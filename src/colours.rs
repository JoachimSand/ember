// TODO: This approach can be improved greatly.
// Might be worth using an external crate for this.
#[macro_export]
macro_rules! black {
    ( $l:literal) => {{
        colour!($l, "black")
    }}
}

#[macro_export]
macro_rules! red {
    ( $l:literal) => {{
        colour!($l, "red")
    }}
}

#[macro_export]
macro_rules! green {
    ( $l:literal) => {{
        colour!($l, "green")
    }}
}


#[macro_export]
macro_rules! yellow {
    ( $l:literal) => {{
        colour!($l, "yellow")
    }}
}

#[macro_export]
macro_rules! blue {
    ( $l:literal) => {{
        colour!($l, "blue")
    }}
}

#[macro_export]
macro_rules! magenta {
    ( $l:literal) => {{
        colour!($l, "magenta")
    }}
}

#[macro_export]
macro_rules! cyan {
    ( $l:literal) => {{
        colour!($l, "cyan")
    }}
}

#[macro_export]
macro_rules! white {
    ( $l:literal) => {{
        colour!($l, "white")
    }}
}

#[macro_export]
macro_rules! default {
    ( $l:literal) => {{
        colour!($l, "default")
    }}
}

#[macro_export]
macro_rules! b_black {
    ( $l:literal) => {{
        colour!($l, "b_black")
    }}
}

#[macro_export]
macro_rules! b_red {
    ( $l:literal) => {{
        colour!($l, "b_red")
    }}
}

#[macro_export]
macro_rules! b_green {
    ( $l:literal) => {{
        colour!($l, "b_green")
    }}
}

#[macro_export]
macro_rules! b_yellow {
    ( $l:literal) => {{
        colour!($l, "b_yellow")
    }}
}

#[macro_export]
macro_rules! b_blue {
    ( $l:literal) => {{
        colour!($l, "b_blue")
    }}
}

#[macro_export]
macro_rules! b_magenta {
    ( $l:literal) => {{
        colour!($l, "b_magenta")
    }}
}

#[macro_export]
macro_rules! b_cyan {
    ( $l:literal) => {{
        colour!($l, "b_cyan")
    }}
}

#[macro_export]
macro_rules! d_white {
    ( $l:literal) => {{
        colour!($l, "d_white")
    }}
}

#[macro_export]
macro_rules! colour {
    ( $l:expr, $c:literal) => {{
        let ret : &str = match $c {
            "black" =>      concat!("\x1b[1;30m", $l, "\x1b[0m"),
            "red" =>        concat!("\x1b[1;31m", $l, "\x1b[0m"),
            "green" =>      concat!("\x1b[1;32m", $l, "\x1b[0m"),
            "yellow" =>     concat!("\x1b[1;33m", $l, "\x1b[0m"),
            "blue" =>       concat!("\x1b[1;34m", $l, "\x1b[0m"),
            "magenta" =>    concat!("\x1b[1;35m", $l, "\x1b[0m"),
            "cyan" =>       concat!("\x1b[1;36m", $l, "\x1b[0m"),
            "white" =>      concat!("\x1b[1;37m", $l, "\x1b[0m"),
            "default" =>    concat!("\x1b[1;39m", $l, "\x1b[0m"),
            
            // bright colours
            "b_black" =>    concat!("\x1b[1;90m", $l, "\x1b[0m"),
            "b_red" =>      concat!("\x1b[1;91m", $l, "\x1b[0m"),
            "b_green" =>    concat!("\x1b[1;92m", $l, "\x1b[0m"),
            "b_yellow" =>   concat!("\x1b[1;93m", $l, "\x1b[0m"),
            "b_blue" =>     concat!("\x1b[1;94m", $l, "\x1b[0m"),
            "b_magenta" =>  concat!("\x1b[1;95m", $l, "\x1b[0m"),
            "b_cyan" =>     concat!("\x1b[1;96m", $l, "\x1b[0m"),
            "b_white" =>    concat!("\x1b[1;97m", $l, "\x1b[0m"),
            
            // dim colours
            "d_white" =>    concat!("\x1b[1;37;1m", $l, "\x1b[0m"),
            _ =>            concat!("\x1b[0;0m", $l, "\x1b[0m"),
        };
        ret
    }}
}
pub(crate) use {
    black, red, green, yellow, blue, magenta, cyan, white, default, 
    b_black, b_red, b_green, b_yellow, b_blue, b_magenta, b_cyan,
    d_white, 
    colour
};
