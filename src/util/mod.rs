pub fn collect_until_good_error<X, T, E>(f: fn(&mut X) -> Result<T,E>, v: &mut X, good : E)
     -> Result<Vec<T>, E>
     where E : PartialEq {
  let mut res = Vec::new();
  let e = loop {
    let k = f(v);
    if let Ok(r) = k {
      res.push(r);
    } else if let Err(e) = k {
      break e;
    }
  };
  if e == good { Ok(res) }
  else         { Err(e)  }
}

pub fn collect_until_none<X, T>(f: fn(&mut X) -> Option<T>, v: &mut X) -> Vec<T> {
  let mut res = Vec::new();
  loop {
    let k = f(v);
    if let Some(r) = k {
      res.push(r);
    } else {
      break;
    }
  };
  res
}
