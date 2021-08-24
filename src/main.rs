#![feature(iter_advance_by)]
#![feature(ptr_internals)]
#![feature(const_generics)]
#![feature(box_syntax)]

mod lexer;
mod parser;
mod util;
mod language;

use lexer::Lexer;
use parser::Parser;
use peekmore::PeekMore;
use std::error::Error;
use std::fmt::{Debug, Formatter, Display};

extern crate parse_macro;

fn main() {
  let args: Vec<String> = std::env::args().collect();
  if args.len() != 2 {
    eprintln!("Usage {} <filename>", args[0]);
    std::process::exit(-1);
  }
  let st = std::fs::read_to_string(args[1].clone()).unwrap_or_else(|_| { eprintln!("Couldn't read file {}", args[1]); std::process::exit(-1); });
  let lexed_output = Lexer::new(st).collect_all();
  let parsed_output = Parser::new(lexed_output.unwrap().into_iter().peekmore()).next_function();
  println!("{:?}", parsed_output);
}
