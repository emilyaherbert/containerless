let containerless = require('../dist/index');

let maze = 
[
  [ 0,  0,  0,  0,  0,  0,  0,  0, 99, 99],
  [ 0, 99,  0,  0, 99, 99, 99, 99, 99,  0],
  [ 0, 99,  0,  0, 99,  0,  0,  0, 99,  0],
  [ 0, 99,  0, 99, 99,  0,  0, 99, 99,  0],
  [ 0, 99,  0, 99,  0, 99,  0, 99,  0,  0],
  [ 0, 99, 99, 99,  0, 99,  0, 99,  0,  0],
  [99, 99,  0,  0,  0, 99, 99, 99,  0, 99],
  [99,  0,  0,  0,  0, 99,  0, 99,  0, 99],
  [ 0, 99, 99, 99, 99, 99,  0, 99,  0, 99],
  [ 0,  0,  0,  0,  0,  0,  0, 99, 99, 99],
];

function isValidMove(x, y, turn) {
    return (x > -1) && (y > -1) && (x < maze[0].length) && (y < maze.length) && (maze[y][x] > 0) && (turn < maze[y][x]);
}

function findShortestPath(x1, y1, x2, y2) {
    let shortest = -1;

    let moves = [[x1, y1, 0], [x1-1, y1, 0], [x1+1, y1, 0], [x1, y1-1, 0], [x1, y1+1, 0]];

    while(moves.length > 0) {
        let nextMove = moves.shift();
        let x = nextMove[0];
        let y = nextMove[1];
        let turn = nextMove[2] + 1;
        if(isValidMove(x, y, turn)) {
            if(x === x2 && y === y2) {
                shortest = turn;
            } else {
                maze[y][x] = turn;
                moves.push([x-1, y, turn]);
                moves.push([x+1, y, turn]);
                moves.push([x, y-1, turn]);
                moves.push([x, y+1, turn]);
            }
        }
    }

    return shortest;
}

containerless.listen(function(req) {
    let x1 = req.body.x1;
    let y1 = req.body.y1;
    let x2 = req.body.x2;
    let y2 = req.body.y2;
    if(isValidMove(x1, y1, 0) && isValidMove(x2, y2, 0)) {
        containerless.respond(findShortestPath(1, 1, 7, 9) + "\n");
    } else {
        containerless.respond("Invalid starting conditions.\n");
    }
});