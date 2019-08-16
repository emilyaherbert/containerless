import * as helpers from '../helpers';

test('objects 1', () => {
  let code = `
  function main(x) {
    let y = { a : 1 };
    return y.a;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 2', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return y.a;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 3', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return y.b;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 4', () => {
  let code = `
  function F(a) {
    return { x : 100, z : 1000 };
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    return y.x;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects 5', () => {
  let code = `
  function F(a) {
    return { x : 100, z : 1000 };
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    return y.z;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('object modification 1', () => {
  let code = `
  function main(x) {
    let y = { a : 1 };
    y.a = 2;
    return y.a;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('object modification 2', () => {
  let code = `
  function F(a) {
    return { x : 100, z : 1000 };
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    y.z = 2000;
    return y.z;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('object modification 3', () => {
  let code = `
  function F(a) {
    let b = { x : 100, z : 1000 };
    b.x = 200;
    return b;
  }

  function main(x) {
    let a = 2;
    let y = F(a);
    return y.x;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('object modification 4', () => {
  let code = `
  function main(input) {
    let a = input;
    let mine = { x : a };
    a = -input;
    return mine.x;
  }
  main
  `;

  let input = 100;
  helpers.run_test(code, input);
});

test('objects in operations 1', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return y.a + y.b;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects in operations 2', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2 };
    return (y.a > 0);
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('objects in operations 2', () => {
  let code = `
  function main(x) {
    let y = { a : 1, b : 2, c : true };
    if (y.c) {
      return (y.a - y.b);
    } else {
      return (y.b - y.a);
    }
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});

test('multiple objects 1', () => {
  // One class.

  let code = `
  function main(input) {
    let mine = { x : input, y : 76 };
    let theirs = { x : -input, y : 17 };
    if(mine.x > theirs.x) {
      return mine.y - theirs.y;
    } else {
      return theirs.y - mine.y;
    }
  }
  main
  `;

  let input = 100;
  helpers.run_test(code, input);
});

test('multiple objects 2', () => {
  // Two classes.
  
  let code = `
  function main(input) {
    let mine = { x : input, y : 76 };
    let theirs = { x : -input, y : 17 };
    let others = { x : (input * input), z : 43 };
    if(mine.x > theirs.x) {
      return mine.y - theirs.y;
    } else {
      return theirs.y - mine.y;
    }
  }
  main
  `;

  let input = 100;
  helpers.run_test(code, input);
});

test('multiple objects 3', () => {
  // Two classes.
  
  let code = `
  function main(input) {
    let mine = { x : input, y : 76 };
    let theirs = { x : -input, y : 17 };
    let others = { x : (input * input), z : 43 };
    let whose = { y : 1000000, x : 2 };
    if(mine.x > theirs.x) {
      return mine.y - theirs.y;
    } else {
      return theirs.y - mine.y;
    }
  }
  main
  `;

  let input = 100;
  helpers.run_test(code, input);
});

test('multiple objects 4', () => {
  // Two classes.
  
  let code = `
  function main(input) {
    let mine = { x : input, y : 76 };
    let theirs = { x : -input, y : 17 };
    let others = { x : (input * input), z : 43 };
    let whose = { y : 1000000, x : 2 };
    if(mine.x > theirs.x) {
      return mine.y - theirs.y;
    } else {
      return theirs.y - mine.y;
    }
  }
  main
  `;

  let input = -100;
  helpers.run_test(code, input);
});

test('objects and functions 1', () => {
  let code = `
  function createObject(a, b) {
    return { x : a, y : b };
  }

  function main(input) {
    let a = 100;
    let b = input;
    let c = 10000;
    let alices = createObject(a, b);
    let bobs = createObject(b, c);
    let eves = createObject(a, c);
    if(alices.y > eves.x) {
      return bobs.y;
    } else {
      return bobs.x;
    }
  }
  main
  `;

  let input = 1000;
  helpers.run_test(code, input);
});

test('hidden classes 1', () => {
  let code = `
  function createObject(a, b) {
    return { x : a, y : b };
  }

  function main(input) {
    let a = 100;
    let b = input;
    let c = 10000;
    let alices = createObject(a, b);
    let bobs = createObject(b, c);
    alices.z = true;
    if(alices.z) {
      return bobs.y;
    } else {
      return bobs.x;
    }
  }
  main
  `;

  let input = 1000;
  helpers.run_test(code, input);
});

test('hidden classes 2', () => {
  let code = `
  function createObject(a, b) {
    return { x : a, y : b };
  }

  function main(input) {
    let a = 100;
    let b = input;
    let c = 10000;
    let alices = createObject(a, b);
    let bobs = createObject(b, c);
    alices.z = true;
    bobs.z = 1;
    if(alices.z) {
      return bobs.z;
    } else {
      return bobs.x;
    }
  }
  main
  `;

  let input = 1000;
  helpers.run_test(code, input);
});

// TODO(emily): This does not pass because the else is 'unknown' in the AST.
// This should hopefully be resolved when we account for multiple inputs.
/*
test('objects and loops 1', () => {
  let code = `
  function main(x) {
    let count = 0;
    let y = { a : 0, b : 10, c : true };
    while(y.c) {
      if(y.a > y.b) {
        y.c = false;
      } else {
        count = count + 25;
        y.a = y.a + 1;
      }
    }
    return count;
  }
  main
  `;

  let input = -1;
  helpers.run_test(code, input);
});
*/