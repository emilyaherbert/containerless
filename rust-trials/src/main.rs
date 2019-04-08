
#![allow(dead_code)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

use std::fmt;

/*

The current paradigm relies heavily on turning references to
owned values. This is only possible if you copy or clone the value.

Rust does this automatically for most things, but we have to derive
it specifically.

*/

#[derive(Clone)]
struct Point {
  x : i32,
  y : i32
}

#[derive(Clone)]
struct TwoPoints {
  p1 : Point,
  p2 : Point
}

#[derive(Clone)]
enum Value {
    Int(i32),
    Bool(bool),
    Stringg(String),
    Arr(Box<Vec<Value>>),
    VPoint(Point),
    VTwoPoints(TwoPoints)
}

impl PartialEq for Value {
  fn eq(&self, other: &Value) -> bool {
    match self {
      Value::Int(x) => {
        match other {
          Value::Int(y) => return x == y,
          _ => panic!("Found different Value types.")
        }
      },
      _ => panic!("Found unimplemented Value type in PartialEq.")
    }
  }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      match self {
        Value::Int(x) => write!(f, "Value::Int({})", x),
        _ => panic!("Found unimplemented Value type in Display.")
      }
    }
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

  fn alloc_array(&mut self, v: Vec<Value>) -> usize {
    self.values.push(Value::Arr(Box::new(v)));
    return self.values.len() - 1;
  }

  fn alloc_vpoint(&mut self, v: Point) -> usize {
    self.values.push(Value::VPoint(v));
    return self.values.len() - 1;
  }

  fn alloc_vtwopoints(&mut self, v: TwoPoints) -> usize {
    self.values.push(Value::VTwoPoints(v));
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

  fn read_vpoint(&mut self, addr: usize) -> &mut Point {
    match self.read(addr) {
      Value::VPoint(p) => return p,
      _ => panic!("oops")
    }
  }

  fn read_vtwopoints(&mut self, addr: usize) -> &mut TwoPoints {
    match self.read(addr) {
      Value::VTwoPoints(p) => return p,
      _ => panic!("oops")
    }
  }

  fn read_imm(&self, addr: usize) -> Value {
    // TODO(emily): Think about if this will cause issues.
    return self.values[addr].clone();
  }

  fn read_int_imm(&self, addr: usize) -> i32 {
    match self.read_imm(addr) {
      Value::Int(b) => return b,
      _ => panic!("Expected to read Int.")
    }
  }

  fn read_bool_imm(&self, addr: usize) -> bool {
    match self.read_imm(addr) {
      Value::Bool(b) => return b,
      _ => panic!("Expected to read Bool.")
    }
  }

  fn read_string_imm(&self, addr: usize) -> String {
    match self.read_imm(addr) {
      Value::Stringg(s) => return s,
      _ => panic!("Expected to read Stringg.")
    }
  }

  fn read_array_imm(&self, addr: usize) -> Vec<Value> {
    match self.read_imm(addr) {
      Value::Arr(vec) => return *vec,
      _ => panic!("oops")
    }
  }

  fn read_vpoint_imm(&self, addr: usize) -> Point {
    match self.read_imm(addr) {
      Value::VPoint(p) => return p,
      _ => panic!("oops")
    }
  }

  fn read_vtwopoints_imm(&self, addr: usize) -> TwoPoints {
    match self.read_imm(addr) {
      Value::VTwoPoints(p) => return p,
      _ => panic!("oops")
    }
  }

}

fn int_test(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if pool.read_int_imm(a) > pool.read_int_imm(b) {
    *pool.read_int(c) = 42;
  } else {
    *pool.read_int(c) = 24;
  }

  return pool.read_int_imm(c) == 42;
}

fn int_test2(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if pool.read_int_imm(a) < pool.read_int_imm(b) {
    *pool.read_int(c) = 42;
  } else {
    *pool.read_int(c) = 24;
  }

  return pool.read_int_imm(c) == 24;
}

fn int_test3(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if pool.read_int_imm(a) > pool.read_int_imm(b) {
    *pool.read_int(c) = pool.read_int_imm(a) + pool.read_int_imm(b);
  } else {
    *pool.read_int(c) = pool.read_int_imm(a) - pool.read_int_imm(b);
  }

  return pool.read_int_imm(c) == 199;
}

fn int_test4(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_int(100);
  let mut b = pool.alloc_int(99);

  let mut c = pool.alloc_int(0);
  
  if pool.read_int_imm(a) < pool.read_int_imm(b) {
    *pool.read_int(c) = pool.read_int_imm(a) + pool.read_int_imm(b);
  } else {
    *pool.read_int(c) = pool.read_int_imm(a) - pool.read_int_imm(b);
  }

  return pool.read_int_imm(c) == 1;
}

fn string_test(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_string(String::from("true"));

  return pool.read_string_imm(a) == "true";
}

fn string_test2(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_string(String::from("e"));

  while (pool.read_string_imm(a)).len() < 10 {
    pool.read_string(a).push_str("e");
    // This also works:
    // (*pool.read_string(a)).push_str("e");
    // but is not necessary because .push_str() automatically dereferences.
  }

  return pool.read_string_imm(a) == "eeeeeeeeee";
}

fn array_test(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_array(vec![]);

  for x in 0..10 {
    pool.read_array(a).push(Value::Int(x));
  }

  return pool.read_array(a).len() == 10;
}

fn array_test2(pool: &mut Pool) -> bool {
  let mut a = pool.alloc_array(vec![]);

  for x in 0..10 {
    pool.read_array(a).push(Value::Int(x));
  }

  //println!("{}", pool.read_array(a)[5]);
  return pool.read_array(a)[5] == Value::Int(5);
}

fn obj_test(pool: &mut Pool) -> bool {
  let p = Point { x:2, y:3 };
  let obj = pool.alloc_vpoint(p);
  (*pool.read_vpoint(obj)).x = 100;

  return pool.read_vpoint_imm(obj).x == 100;
}

fn obj_test2(pool: &mut Pool) -> bool {
  let p1 = Point { x:2, y:3 };
  let obj1 = pool.alloc_vpoint(p1);

  let p2 = Point { x:200, y:300 };
  let obj2 = pool.alloc_vpoint(p2);

  /*
  { 2, 3 }
  { 200, 300 }
  */

  (*pool.read_vpoint(obj1)).x = 100;
  (*pool.read_vpoint(obj2)).y = pool.read_vpoint_imm(obj1).y;

  /*
  { 100, 3 }
  { 200, 3 }
  */

  return pool.read_vpoint_imm(obj1).x + pool.read_vpoint_imm(obj2).x == 300
         && pool.read_vpoint_imm(obj1).y + pool.read_vpoint_imm(obj2).y == 6;
}

fn obj_test3(pool: &mut Pool) -> bool {
  let p1 = Point { x:2, y:3 };
  let p2 = Point { x:200, y:300 };
  let p = TwoPoints { p1: p1, p2: p2 };

  /*
  { 2, 3 }
  { 200, 300 }
  */

  let obj = pool.alloc_vtwopoints(p);

  return pool.read_vtwopoints_imm(obj).p1.x == 2;
}

fn obj_test4(pool: &mut Pool) -> bool {
  let p1 = Point { x:2, y:3 };
  let p2 = Point { x:200, y:300 };
  let p = TwoPoints { p1: p1, p2: p2 };

  /*
  { 2, 3 }
  { 200, 300 }
  */

  let obj = pool.alloc_vtwopoints(p);

  (*pool.read_vtwopoints(obj)).p1.x = 4000;
  (*pool.read_vtwopoints(obj)).p2.y = 4000;

  return pool.read_vtwopoints_imm(obj).p1.x == pool.read_vtwopoints_imm(obj).p2.y;
}

fn main() {
  let mut pool = Pool { values: vec![] };

  println!("{}", int_test(&mut pool));
  println!("{}", int_test2(&mut pool));
  println!("{}", int_test3(&mut pool));
  println!("{}", int_test4(&mut pool));

  println!("{}", string_test(&mut pool));
  println!("{}", string_test2(&mut pool));

  println!("{}", array_test(&mut pool));
  println!("{}", array_test2(&mut pool));

  println!("{}", obj_test(&mut pool));
  println!("{}", obj_test2(&mut pool));
  println!("{}", obj_test3(&mut pool));
  println!("{}", obj_test4(&mut pool));
}