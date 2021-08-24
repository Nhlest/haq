use peekmore::PeekMoreIterator;
use crate::lexer::Token;
use crate::parser::FunctionU;
use std::vec::IntoIter;
use crate::parser::ParserError::*;

#[derive(Debug)]
pub enum ParserError {
  FunctionNameMissing,
  FunctionMissing,
  TypeStackMissing,
  TypeOrTypeStackMissing,
  TypeMissing,
  TokenMissing(Token),
  UnknownType(String)
}

pub struct Parser {
  stream: PeekMoreIterator<IntoIter<Token>>,
}

use parse_macro::make_answer;
make_answer!(23*, Token::P);

impl Parser {
  pub fn new(stream: PeekMoreIterator<IntoIter<Token>>) -> Self {
    Parser {
      stream
    }
  }
  fn next(&mut self) -> Option<Token> {
    self.stream.next()
  }
  fn peek(&mut self) -> Option<Token> {
    self.stream.next()
  }
  pub fn next_function(&mut self) -> Result<FunctionU, ParserError> {
    // let r = expect!(self, 1);
    Err(FunctionNameMissing)
  }
}