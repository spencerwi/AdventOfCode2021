import ballerina/io;
import ballerina/regex;

type Point record {
    int x;
    int y;
};
function parsePoint(string input) returns Point {
    int[] components = 
        from string component in regex:split(input, ",")
        select checkpanic int:fromString(component);
    return {x: components[0], y: components[1]};
}

type Line record {
    Point begin;
    Point end;
};
function parseLine(string input) returns Line {
    Point[] points = 
        from string pointStr in regex:split(input, " -> ")
        select parsePoint(pointStr);
    return {begin: points[0], end: points[1]};
}
function isVertical(Line line) returns boolean {
    return line.begin.x == line.end.x;
}
function isHorizontal(Line line) returns boolean {
    return line.begin.y == line.end.y;
}
function getIncrement(int begin, int end) returns int {
    if (begin < end) { 
        return 1;
    } else if (begin > end) {
        return -1;
    } else {
        return 0;
    }
}

class Grid {
    private int[][] data;

    function init(int height, int width) {
        self.data = [];
        foreach int x in 0..<height {
            int[] row = 
                from int y in 0..<width
                select 0;
            self.data.push(row);
        }
    }

    function drawLine(Line line) {
        int xIncrement = getIncrement(line.begin.x, line.end.x);
        int yIncrement = getIncrement(line.begin.y, line.end.y);
        var {x,y} = line.begin;
        while (x != (line.end.x + xIncrement) || (y != (line.end.y + yIncrement))) {
            self.data[y][x] += 1;
            x += xIncrement;
            y += yIncrement;
        }
    }

    function countOverlaps() returns int {
        int[] overlapPoints =
            from int[] row in self.data
            from int point in row
            where point > 1
            select point;
        return overlapPoints.length();
    }
}

function solve(Line[] input) returns [int, int] {
    int gridHeight = int:max(0, ...(
        from Line line in input
        let var {begin, end} = line
        select int:max(begin.y, end.y)
    )) + 1;
    int gridWidth = int:max(0, ...(
        from Line line in input
        let var {begin, end} = line
        select int:max(begin.x, end.x)
    )) + 1;
    Grid grid = new Grid(height = gridHeight, width = gridWidth);

    // Part A: only draw horizontal/vertical lines, then count overlaps
    foreach Line line in input {
        if isHorizontal(line) || isVertical(line) {
            grid.drawLine(line);
        }
    }
    int partA = grid.countOverlaps();

    // Part B: now draw the diagonal ones, and count overlaps again
    foreach Line line in input {
        if !isHorizontal(line) && !isVertical(line) {
            grid.drawLine(line);
        }
    }
    return [partA, grid.countOverlaps()];
}

public function main() {
    Line[] inputLines = 
        from string lineSpec in checkpanic io:fileReadLines("input.txt")
        select parseLine(lineSpec);
    
    var [partA, partB] = solve(inputLines);

    io:println("Part A: ", partA);
    io:println("Part B: ", partB);
}
