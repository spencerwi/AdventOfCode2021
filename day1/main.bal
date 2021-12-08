import ballerina/io;

function countIncreases(int[] input) returns int {
    int total = 0;
    int? previous = null;
    foreach int current in input {
        if previous != null && current > previous {
            total += 1;
        }
        previous = current;
    }
    return total;
}

function partA(int[] input) returns int {
    return countIncreases(input);
}

function partB(int[] input) returns int {
    int[] sums = 
        from int i in 2..<input.length()
        select input[i] + input[i - 1] + input[i - 2];
    return countIncreases(sums);
}

public function main() returns error? {
    string[]lines = check io:fileReadLines("input.txt");

    int[] input = 
        from var line in lines
        let int parsed = checkpanic int:fromString(line)
        select parsed;

    io:println("Part A: ", partA(input));
    io:println("Part B: ", partB(input));
}