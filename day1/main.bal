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
    int[] sums = [];
    foreach int i in 2..<input.length() {
        sums.push(input[i] + input[i - 1] + input[i - 2]);
    }
    return countIncreases(sums);
}

public function main() {
    string[]|io:Error lines = io:fileReadLines("input.txt");
    if lines is error {
        io:println("Could not read input.txt; are you sure it's there?");
        return;
    }

    int[] input = from var line in lines
                    let int|error parsed = int:fromString(line)
                    where !(parsed is error)
                    select parsed;

    io:println("Part A: ", partA(input));
    io:println("Part B: ", partB(input));
}