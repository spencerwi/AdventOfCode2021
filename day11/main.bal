import ballerina/io;

public type Point record {
    int row;
    int col;
};

public class Grid {
    private int[][] data;

    public function init(int[][] data) {
        self.data = data;
    }

    public function get(Point p) returns int {
        return self.data[p.row][p.col];
    }

    public function height() returns int {
        return self.data.length();
    }
    public function width() returns int {
        return self.data[0].length();
    }

    public function isFlashing(Point p) returns boolean {
        return (self.data[p.row][p.col] > 9);
    }

    public function neighborsOf(Point p) returns Point[] {
        return 
            from int row in (p.row - 1)...(p.row + 1) 
            from int col in (p.col - 1)...(p.col + 1) 
            where row >= 0 && row < self.height()
            where col >= 0 && col < self.width()
            select {row, col};
    }

    public function flashingCells() returns Point[] {
        return 
            from int row in 0..<self.height()
            from int col in 0..<self.width()
            let Point p = {row, col}
            where self.isFlashing(p)
            select p;
    }

    public function increment(Point p) {
        self.data[p.row][p.col] += 1;
    }

    public function step() returns int {
        Point[] flashesToProcess = [];
        Point[] alreadyFlashedCells = [];

        // First, increment every cell
        foreach int row in 0..<self.height() {
            foreach int col in 0..<self.width() {
                Point p = {row, col};
                self.increment(p);
                if self.isFlashing(p) {
                    flashesToProcess.push(p);
                }
            }
        }

        // Next, process all the flashing cells by incrementing their 
        // not-yet-flashed  neighbors, then check for more flashes, and process 
        // them, etc, and repeat until there are no new flashing cells.
        while (flashesToProcess.length() > 0) {
            // Process all the flashing cells
            foreach var _ in 0..<flashesToProcess.length() {
                Point currentFlash = flashesToProcess.shift();
                if (alreadyFlashedCells.indexOf(currentFlash) == null) {
                    foreach Point neighbor in self.neighborsOf(currentFlash) {
                        if (alreadyFlashedCells.indexOf(neighbor) == null) {
                            self.increment(neighbor);
                        }
                    }
                    self.data[currentFlash.row][currentFlash.col] = 0;
                    alreadyFlashedCells.push(currentFlash);
                }
            }

            // Now look for cells that are flashing now that haven't already flashed
            flashesToProcess = 
                from Point p in self.flashingCells()
                where alreadyFlashedCells.indexOf(p) == null
                select p;
        }

        return alreadyFlashedCells.length();
    }
}

public function solve(string[] input) {
    int[][] data = input.map(function(string line) returns int[] {
        return
            from string char in line
            select checkpanic int:fromString(char);
    });
    var grid = new Grid(data);
    boolean hasEveryoneFlashedOnThisStep = false;
    int totalFlashesSoFar = 0;
    int stepCounter = 0;
    while !hasEveryoneFlashedOnThisStep {
        stepCounter += 1;
        int flashCount = grid.step();
        totalFlashesSoFar += flashCount;
        if (stepCounter == 100) {
            io:println("Part A: ", totalFlashesSoFar);
        }
        hasEveryoneFlashedOnThisStep = (flashCount == (grid.height() * grid.width()));
    }
    io:println("Part B: ", stepCounter);
}

// Prints `Hello, World!`.

public function main() {
    string[] inputLines = checkpanic io:fileReadLines("input.txt");
    solve(inputLines);
}
