#[derive(Debug)]
pub enum Type {
  Word8,
  Stack(Vec<Type>),
  Arrow(Box<Type>, Box<Type>)
}

impl Type {
  pub fn arrow(left: Type, right: Type) -> Type {
    Type::Arrow(Box::new(left), Box::new(right))
  }
}