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