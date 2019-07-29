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