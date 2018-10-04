var createVertex = require("dynamic-forest");

/* Load a program from handle (presumably stdin) */
function load(handle, ok) {
    var chunks = [];
    handle.resume();
    handle.setEncoding('utf8');
    handle.on('data', function (chunk) {
        chunks.push(chunk);
    });

    handle.on('end', function () {
        ok(JSON.parse(chunks.join('')));
    });
}

var TAG_INSERT = 0;
var TAG_LINK = 1;
var TAG_DELETE = 2;
var TAG_CUT = 3;
var TAG_CONNECTED = 4;

/* Run a program */
function run(program) {
    var vertices = {};
    var edges = {};

    for (var i = 0; i < program.length; i++) {
        var instruction = program[i];

        if (instruction.tag == TAG_INSERT) {
            var x = instruction.x;
            vertices[x] = createVertex(x);
            edges[x] = {};

        } else if (instruction.tag == TAG_LINK) {
            var x = instruction.x;
            var y = instruction.y;
            var edge = vertices[x].link(vertices[y]);

            edges[x][y] = edge;
            edges[y][x] = edge;

        } else if (instruction.tag == TAG_DELETE) {
            var x = instruction.x;
            vertices[x].cut();

        } else if (instruction.tag == TAG_CUT) {
            var x = instruction.x;
            var y = instruction.y;
            var edge = edges[x][y];
            edge.cut();

        } else if (instruction.tag == TAG_CONNECTED) {
            var x = instruction.x;
            var y = instruction.y;
            var c = vertices[x].connected(vertices[y]);
            if (c !== instruction.expect) {
                console.log('Expect: ' + instruction.expect);
                console.log('Actual: ' + c);
                throw "Unexpected result from connected at instruction " + i;
            }

        } else {
            throw "Unknown tag: " + instruction.tag;
        }
    }
}

/* Main-ish */
load(process.stdin, function(program) {
    run(program);
});
