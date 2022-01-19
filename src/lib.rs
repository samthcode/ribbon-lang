pub mod cli;
pub mod lexer;
pub mod interp;
pub use interp::Interpreter;
pub mod pos;
pub mod error;