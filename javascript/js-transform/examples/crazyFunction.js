function outer(x) {
  function inner1(y) {
    return (x*100) + (y*10);
  }

  function inner2(y,z) {
    return inner1(y) + z;
  }

  if(x>0) {
    let inner1 = 1;
  }

  return inner2;
}

let F = outer(3);
let G = F(4, 5)
