import ballerina/io;
import ballerina/regex;

public type CellState "marked"|"unmarked";
public type Cell record {
    int value;
    CellState state;
};
public type BingoBoard Cell[][];

// Ballerina doesn't have generic support, and its @typeParam thing doesn't work right
// so this accepts string[] and returns string[] instead of accepting T[] and returning T[]
function chunkBy(string[] input, int size) returns string[][] {
    string[][] result = [];
    string[] current = [];
    foreach int i in 0..<input.length() {
        if (i > 0 && (i % size == 0)) {
            result.push(current);
            current = [];
        }
        current.push(input[i]);
    }
    return result;
}

public function columns(BingoBoard board) returns Cell[][] {
    Cell[][] result = [];
    foreach int i in 0..<board[0].length() {
        result.push(
            from Cell[] row in board 
            select row[i]
        );
    }
    return result;
}

function markCell(int number, BingoBoard board) {
    foreach int rowNum in 0..<board.length() {
        Cell[] row = board[rowNum];
        foreach int colNum in 0..<row.length() {
            if (row[colNum].value == number) {
                board[rowNum][colNum] = {value: number, state: "marked"};
            }
        }
    }
}

function boardHasWon(BingoBoard board) returns boolean {
    var isWinningSequence = function(Cell[] sequence) returns boolean {
        foreach var {state} in sequence {
            if state == "unmarked" {
                return false;
            }
        }
        return true;
    };
    foreach Cell[] row in board {
        if isWinningSequence(row) { return true; }
    }
    foreach Cell[] column in columns(board) {
        if isWinningSequence(column) { return true; }
    }
    return false;
}

function score(BingoBoard board, int lastNumberCalled) returns int {
    int[] unmarkedCellValues = 
        from Cell[] row in board
        from Cell cell in row
        where cell.state == "unmarked"
        select cell.value;

    int unmarkedCellSum = unmarkedCellValues.reduce(
        function(int a, int b) returns int {
            return a + b;
        },
    0);

    return unmarkedCellSum * lastNumberCalled;
}


public function main() returns error? {
    string[] lines = checkpanic io:fileReadLines("input.txt");
    int[] numbersCalled = 
        from string substring in regex:split(lines[0], ",")
        select checkpanic int:fromString(substring);

    string[] nonEmptyBingoLines = 
        from int i in 1..<lines.length()
        let string line = lines[i]
        where line.trim().length() > 0
        select line;

    BingoBoard[] boards = chunkBy(nonEmptyBingoLines, 5)
        .map(function (string[] boardLines) returns BingoBoard {
            return boardLines.map(function (string line) returns Cell[] {
                return 
                    from string element in regex:split(line, "\\s+")
                    where element.trim().length() > 0
                    let int intElement = checkpanic int:fromString(element)
                    select {value: intElement, state: "unmarked"};
            });
        });

    int[] winningScores = [];
    BingoBoard[] boardsLeft = boards;
    int numberIdx = 0;
    while boardsLeft.length() > 0 {
        int nextNumber = numbersCalled[numberIdx];
        foreach BingoBoard board in boardsLeft {
            markCell(nextNumber, board);
            if (boardHasWon(board)) {
                winningScores.push(score(board, nextNumber));
            }
        }
        boardsLeft = 
            from BingoBoard board in boardsLeft
            where !boardHasWon(board)
            select board;
        numberIdx += 1;
    }

    io:println("Part A:", winningScores[0]);
    io:println("Part B:", winningScores[winningScores.length() - 1]);
}
