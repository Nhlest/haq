use std::vec::IntoIter;
use crate::lexer::{Token, LexingError};
use crate::util::{collect_until_good_error, collect_until_none};
use std::str::FromStr;
use peekmore::*;

pub struct Lexer {
  stream: PeekMoreIterator<IntoIter<char>>,
  eol: bool,
  indentation: Vec<usize>,
  unwind: usize,
  meaningful_line: bool
}

impl Lexer {
  pub fn new(input: String) -> Self {
    Lexer {
      stream: input.as_str().chars().collect::<Vec<_>>().into_iter().peekmore(),
      eol: true,
      indentation: Vec::new(),
      unwind: 0,
      meaningful_line: false
    }
  }
  fn ok(&mut self, t: Token) -> Result<Token, LexingError> {
    self.stream.next();
    self.meaningful_line = true;
    Ok(t)
  }
  pub fn next_token(&mut self) -> Result<Token, LexingError> {
    if self.unwind > 0 {
      self.unwind-=1;
      Ok(Token::IndentationDown)
    } else if let Some(c) = self.stream.peek().cloned() {
      if self.eol {
        self.eol = false;
        self.meaningful_line = false;
        let whitespace = collect_until_none(|v|Lexer::next_if(v, |c|c==' '), self);
        // TODO: FIXME: TODO: FIXME: TODO: FIXME: PLEASE FIX ME
        if self.stream.peek().cloned().unwrap_or(' ') == '\n'
          || self.stream.peek().cloned().unwrap_or(' ') == '-' && self.stream.peek().cloned().unwrap_or(' ') == '-' {
          return self.next_token();
        }
        let mut new_indent = whitespace.len();
        let old_indent = self.indentation.iter().sum();
        if new_indent == old_indent {
          self.next_token()
        } else if new_indent > old_indent {
          self.indentation.push(new_indent - old_indent);
          Ok(Token::IndentationUp)
        } else {
          loop {
            if let Some(i) = self.indentation.pop() {
              self.unwind+=1;
              if self.indentation.iter().sum::<usize>() < new_indent {
                break Err(LexingError::IndentationMismatch);
              } else if new_indent == self.indentation.iter().sum::<usize>() {
                break self.next_token();
              }
            } else {
              break self.next_token();
            }
          }
        }
      } else if c == '\n' {
        self.eol = true;
        self.stream.advance_by(1).unwrap();
        if self.meaningful_line {
          Ok(Token::Newline)
        } else {
          self.next_token()
        }
      } else if c.is_whitespace() {
        self.stream.advance_by(1).unwrap();
        self.next_token()
      } else if (c == '-') && (*self.stream.peek_next().unwrap_or(&' ') == '-') {
        collect_until_none(|v|Lexer::next_if(v, |c|c!='\n'), self);
        self.next_token()
      } else if c.is_numeric() {
        self.meaningful_line = true;
        let n = collect_until_none(|v| Lexer::next_if(v, char::is_numeric), self);
        match u32::from_str(n.into_iter().collect::<String>().as_str()) {
          Ok(n) => Ok(Token::Number(n)),
          Err(e) => {
            eprintln!("{}", e);
            Err(LexingError::NumericParsingError)
          }
        }
      } else if c.is_alphanumeric() {
        self.meaningful_line = true;
        let n = collect_until_none(|v| Lexer::next_if(v, char::is_alphanumeric), self);
        let hash = self.stream.peek().cloned() == Some('#');
        if hash {
          self.stream.next();
          Ok(Token::HashLiteral(n.into_iter().collect()))
        } else {
          Ok(Token::Literal(n.into_iter().collect()))
        }
      } else if c == ':' { self.ok(Token::Colon)
      } else if c == '[' { self.ok(Token::OSBracket)
      } else if c == ']' { self.ok(Token::CSBracket)
      } else if c == ',' { self.ok(Token::Comma)
      } else if c == '-' && (*self.stream.peek().unwrap_or(&' ') == '>') { self.stream.next(); self.ok(Token::ArrowRight)
      } else { Err(LexingError::UnknownCharacter(c)) }
    } else {
      if !self.indentation.is_empty() {
        self.unwind = self.indentation.len();
        self.indentation.clear();
        self.next_token()
      } else {
        if self.meaningful_line {
          self.meaningful_line = false;
          Ok(Token::Newline)
        } else {
          Err(LexingError::EOF)
        }
      }
    }
  }
  pub fn next_if(&mut self, predicate: fn(char) -> bool) -> Option<char> {
    match self.stream.peek().cloned() {
      Some(c) if predicate(c) => {
        self.stream.next();
        Some(c)
      },
      Some(_) => {
        None
      }
      _ => None
    }
  }
  pub fn collect_all(&mut self) -> Result<Vec<Token>, LexingError> {
    Ok(collect_until_good_error(Lexer::next_token, self, LexingError::EOF)?)
  }
}