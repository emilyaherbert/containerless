function F(x) {
    let ret = 0;
    if(x > 10) {
        ret = 42;
    } else {
        ret = 24;
    }
    return ret;
}

let w = F(11);
let v = F(9);

function G(x) {
    let ret = 0;
    if(x > 10) {
        ret = w + 42;
    } else {
        ret = v + 24;
    }
    return ret;
}

let a = G(11);
let b = G(9);

function zero() {
    let foo = 0;
    function one(b) {
        foo = foo + b;
        function two(c) {
            foo = foo - c;
            function three() {
                return foo;
            }
            return three;
        }
        return two;
    }
    return one;
}
let add = zero();
let sub = add(15);
let toss = add(1);
let ret = sub(4);
let foo = ret();