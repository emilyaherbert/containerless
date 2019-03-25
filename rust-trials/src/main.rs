enum Value {
    Int(i32),
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

    fn read(&mut self, addr: usize) -> &mut Value {
        return &mut self.values[addr];
    }

    fn read_array(&mut self, addr: usize) -> &mut Vec<Value> {
        match self.read(addr) {
            Value::Arr(vec) => vec,
            _ => panic!("oops")
        }
    }

}


fn main() {
    let mut pool = Pool { values: vec![] };

    let mut b = pool.alloc(Value::Arr(Box::new(vec![Value::Int(10), Value::Int(20)])));
    let mut a = b;
    pool.read_array(a).push(Value::Int(30));
    pool.read_array(b).push(Value::Int(40));
    println!("Hello, world!");
}
