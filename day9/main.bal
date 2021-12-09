import ballerina/io;

type Point record {
    int row;
    int column;
};
type Grid int[][];
type Basin Point[];

function digitsInLine(string line) returns int[] {
    int[] digits = [];
    foreach string character in line {
        digits.push(checkpanic int:fromString(character));
    }
    return digits;
}

function valueAt(Point p, Grid grid) returns int {
    return grid[p.row][p.column];
}

function findNeighbors(Point point, Grid grid) returns Point[] {
    var {row, column} = point;
    Point[] neighborCandidates = [
                           {row: row - 1, column},
    {row, column: column - 1},               {row, column: column + 1},
                           {row: row + 1, column}
    ];
    int height = grid.length();
    int width = grid[0].length();
    return 
        from Point candidate in neighborCandidates
        where candidate != point
        where 0 <= candidate.row && candidate.row < height 
        where 0 <= candidate.column && candidate.column < width
        select candidate;
}

function isLowPoint(Point p, Grid grid) returns boolean {
    foreach Point neighbor in findNeighbors(p, grid) {
        if valueAt(p, grid) > valueAt(neighbor, grid) {
            return false;
        }
    }
    return true;
}

function findBasinStartingWith(Point p, Grid grid) returns Basin {
    // Ballerina's arrays have push/shift, so they work well enough as FIFO queues
    // So we can use them for this BFS
    Point[] pointsToVisit = [p];
    Basin pointsSeen = []; // We have to make sure we don't revisit a point, because then we'd infinitely loop
    while pointsToVisit.length() > 0 {
        Point currentPoint = pointsToVisit.shift();
        if (valueAt(currentPoint, grid) == 9) { continue; }
        if (pointsSeen.indexOf(currentPoint) != null) { continue; }
        pointsSeen.push(currentPoint);
        pointsToVisit.push(...findNeighbors(currentPoint, grid));
    }
    return pointsSeen;
}

function solve(Grid grid) returns [int, int] {
    Point[] lowPoints = 
        from int row in 0..<grid.length()
        from int column in 0..<grid[row].length()
        let Point p = {row, column}
        where isLowPoint(p, grid)
        select p;

    int partA = int:sum(...( 
        from Point p in lowPoints
        select valueAt(p, grid) + 1
    ));

    Basin[] basins = [];
    var isAlreadyInAnotherBasin = function(Point p) returns boolean {
        foreach Basin basin in basins {
            if basin.indexOf(p) != null {
                return true;
            }
        }
        return false;
    };

    foreach Point p in lowPoints {
        if !isAlreadyInAnotherBasin(p) {
            basins.push(findBasinStartingWith(p, grid));
        }
    }

    int[] largestBasinSizes = 
        from Basin b in basins
        order by b.length() descending
        limit 3
        select b.length();

    int partB = largestBasinSizes.reduce(function (int total, int size) returns int { 
        return total * size;
    }, 1);
    
    return [partA, partB];
}

public function main() {
    Grid grid = 
        from string line in checkpanic io:fileReadLines("input.txt")
        select digitsInLine(line);

    var [partA, partB] = solve(grid);
    io:println("Part A: ", partA);
    io:println("Part B: ", partB);
}
