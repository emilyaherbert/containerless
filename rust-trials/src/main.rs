enum Value {
    Int(i32),
    Bool(bool),
    Stringg(String),
    Arr(Box<Vec<Value>>)
}

struct Pool {
    values: Vec<Value>,
}

impl Pool {

    fn alloc(&mut self, v: Value) -> usize {
        self.values.push(v);
        return self.values.len() - 1;
    }

    fn alloc_int(&mut self, v: i32) -> usize {
      self.values.push(Value::Int(v));
      return self.values.len() - 1;
    }

    fn alloc_bool(&mut self, v: bool) -> usize {
      self.values.push(Value::Bool(v));
      return self.values.len() - 1;
    }

    fn alloc_string(&mut self, v: String) -> usize {
      self.values.push(Value::Stringg(v));
      return self.values.len() - 1;
    }

    fn read(&mut self, addr: usize) -> &mut Value {
        return &mut self.values[addr];
    }

    fn read_int(&mut self, addr: usize) -> &mut i32 {
      match self.read(addr) {
        Value::Int(b) => return b,
        _ => panic!("Expected to read Int.")
      }
    }

    fn read_bool(&mut self, addr: usize) -> &mut bool {
      match self.read(addr) {
        Value::Bool(b) => return b,
        _ => panic!("Expected to read Bool.")
      }
    }

    fn read_string(&mut self, addr: usize) -> &mut String {
      match self.read(addr) {
        Value::Stringg(s) => return s,
        _ => panic!("Expected to read Stringg.")
      }
    }

    fn read_array(&mut self, addr: usize) -> &mut Vec<Value> {
        match self.read(addr) {
            Value::Arr(vec) => return vec,
            _ => panic!("oops")
        }
    }

}

/*
  Reassigning to primitives gets transformed directly.
  
  let x = 10;
  x = 11;

  let mut x = pool.alloc_int(10));
  x = pool.alloc_int(11));
*/

fn int_test(mut pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if(*pool.read_int(a) > *pool.read_int(b)) {
    c = pool.alloc_int(42);
  } else {
    c = pool.alloc_int(24);
  }

  let mut c_ = *pool.read_int(c);
  return (c_ == 42);
}

fn int_test2(mut pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if(*pool.read_int(a) < *pool.read_int(b)) {
    c = pool.alloc_int(42);
  } else {
    c = pool.alloc_int(24);
  }

  let mut c_ = *pool.read_int(c);
  return (c_ == 24);
}

/*
  Assigning values using complex operations...
    i.e. anything that involves using the pool.
    is expanded into two statements.
  
  let x = a + b;
  
  let x_ = *pool.read_int(a) + *pool.read_int(b);
  let mut x = pool.alloc_int(x_));
*/

fn int_test3(mut pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if(*pool.read_int(a) > *pool.read_int(b)) {
    let mut c_ = *pool.read_int(a) + *pool.read_int(b);
    c = pool.alloc_int(c_);
  } else {
    let mut c_ = *pool.read_int(a) - *pool.read_int(b);
    c = pool.alloc_int(c_);
  }

  let mut c_ = *pool.read_int(c);
  return (c_ == 199);
}

fn int_test4(mut pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if(*pool.read_int(a) < *pool.read_int(b)) {
    let mut c_ = *pool.read_int(a) + *pool.read_int(b);
    c = pool.alloc_int(c_);
  } else {
    let mut c_ = *pool.read_int(a) - *pool.read_int(b);
    c = pool.alloc_int(c_);
  }

  let mut c_ = *pool.read_int(c);
  return (c_ == 1);
}

fn string_test(mut pool: &mut Pool) -> bool {
  let mut a = pool.alloc_string(String::from("true"));

  return(*pool.read_string(a) == "true");
}

fn string_test2(mut pool: &mut Pool) -> bool {
  let mut a = pool.alloc_string(String::from("e"));

  while((*pool.read_string(a)).len() < 10) {
    pool.read_string(a).push_str("e");
  }

  return (*pool.read_string(a) == "eeeeeeeeee");
}

fn main() {
  let mut pool = Pool { values: vec![] };

  println!("{}", int_test(&mut pool));
  println!("{}", int_test2(&mut pool));
  println!("{}", int_test3(&mut pool));
  println!("{}", int_test4(&mut pool));

  println!("{}", string_test(&mut pool));
  println!("{}", string_test2(&mut pool));
}

/*
fn main() {
    let mut pool = Pool { values: vec![] };

    let mut b = pool.alloc(Value::Arr(Box::new(vec![Value::Int(10), Value::Int(20)])));
    let mut a = b;
    pool.read_array(a).push(Value::Int(30));
    pool.read_array(b).push(Value::Int(40));
    println!("Hello, world!");
}
*/