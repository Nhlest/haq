#[derive(Debug, PartialEq, Eq)]
pub enum Token {
  Literal(String),
  HashLiteral(String),
  Number(u32),
  Comma,
  Colon,
  ArrowRight,
  OSBracket,
  CSBracket,
  Newline,
  IndentationDown,
  IndentationUp,
  Whitespace(usize)
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenG {
  Comma,
  Colon,
  ArrowRight,
  OSBracket,
  CSBracket,
  Newline,
  IndentationDown,
  IndentationUp,
}

impl TokenG {
  fn into_token(self) -> Token {
    match self {
      TokenG::Comma => Token::Comma,
      TokenG::Colon => Token::Colon,
      TokenG::ArrowRight => Token::ArrowRight,
      TokenG::OSBracket => Token::OSBracket,
      TokenG::CSBracket => Token::CSBracket,
      TokenG::Newline => Token::Newline,
      TokenG::IndentationDown => Token::IndentationDown,
      TokenG::IndentationUp => Token::IndentationUp,
    }
  }
}

impl Token {
  pub fn literal(self) -> Option<String> {
    if let Token::Literal(s) = self {
      Some(s.clone())
    } else {
      None
    }
  }
  pub fn tok<const t: TokenG>(self) -> Option<()> {
    if self == t.into_token() {
      Some(())
    } else {
      None
    }
  }
  pub fn tok_ref<const t: TokenG>(&self) -> Option<()> {
    if *self == t.into_token() {
      Some(())
    } else {
      None
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum LexingError {
  UnknownCharacter(char),
  NumericParsingError,
  IndentationMismatch,
  EOF
}