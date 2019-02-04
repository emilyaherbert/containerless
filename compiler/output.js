let ast = [];
let current = ast;
current.push({
    kind: 'let',
    name: 'x',
    expr: {
        kind: 'add',
        left: { kind: 'number', value: 1 },
        right: { kind: 'number', value: 3 }
    }
});
let x = 1 + 3;

function F() {
    current.push({
        kind: 'if',
        test: {
            kind: '>',
            left: { kind: 'identifier', name: 'x' },
            right: { kind: 'number', value: 2 }
        },
        thenPart: 'unknown',
        elsePart: 'unknown'
    });
    if (x > 2) {
        let prev = current[current.length - 1];
        if (prev.thenPart === 'unknown') {
            prev.thenPart = [];
        }
        current = prev.thenPart;
        current.push({
            kind: 'let',
            name: 'y',
            expr: {
                kind: 'mul',
                left: { kind: 'number', value: 20 },
                right: { kind: 'identifier', name: 'x' }
            }
        });
        let y = 20 * x;
    }
    else {
        current.push({
            kind: 'let',
            name: 'z',
            expr: {
                kind: 'mul',
                left: { kind: 'number', value: 5 },
                right: { kind: 'identifier', name: 'x' }
            }
        });  
        let z = 5 * x;

    }
    // Reset current
}

F(900)
console.log(ast);
