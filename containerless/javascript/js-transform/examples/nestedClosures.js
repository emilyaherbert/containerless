function zero() {
    let foo = 0;
    
    function one(b) {
        foo = foo + b;
    
        function two() {
            foo = foo - c;
        
            function three() {
                return foo;
            }

            return three;
        }
    
        return two;
    }
  
    return one;
};

let test = 6;
let add = zero();
let sub = add(15);
let toss = add(1);
let ret = sub(4);
let foo = ret();