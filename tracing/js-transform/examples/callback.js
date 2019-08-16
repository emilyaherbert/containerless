let cb = new Callbacks();
let sender = cb.mockCallback((value) => {
    let ret = 0;
    if (value > 0) {
        ret = 200;
    }
    else {
        ret = -200;
    }
});