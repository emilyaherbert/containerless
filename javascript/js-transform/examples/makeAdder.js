let foo = 10;
let makeAdder = function(x) {
  let inner = function(y) {
    foo = foo + 1;
    return x + y;
  }
  return inner;
}

let F = makeAdder(10);
let bar = F(13);
