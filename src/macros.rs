#[macro_export]

#[cfg(html_trace)]
macro_rules! html_trace {
    ($fmt:expr) => {
         let bt = ::backtrace::Backtrace::new();
         println!( concat!($fmt, " at {:?}"), bt );
    };
    ($fmt:expr, $( $args:expr ),*) => {
         let bt = ::backtrace::Backtrace::new();
         println!( concat!($fmt, " at {:?}"), $( $args ),* , bt );
    };
}
#[cfg(not(html_trace))]
macro_rules! html_trace {
    ($fmt:expr) => {};
    ($fmt:expr, $( $args:expr ),*) => {};
}

#[cfg(html_trace)]
macro_rules! html_trace_quiet {
    ($fmt:expr) => {
         println!( $fmt );
    };
    ($fmt:expr, $( $args:expr ),*) => {
         println!( $fmt, $( $args ),* );
    };
}

#[cfg(not(html_trace))]
macro_rules! html_trace_quiet {
    ($fmt:expr) => {};
    ($fmt:expr, $( $args:expr ),*) => {};
}
