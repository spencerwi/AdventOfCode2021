import ballerina/io;
import ballerina/regex;

function fixedCost(int[] crabs, int targetPosition) returns int {
    return int:sum(...(
        from int crab in crabs
        select int:abs(crab - targetPosition)
    ));
}

// The problem describes a situation where each step
//  costs "itself" in gas -- so step 1 costs 1 gas,
//  step 2 costs 2 gas, etc, and you add them together.
// So to move 3 steps, it costs 1 + 2 + 3 = 6 gas.
// As it turns out, this "factorial but with addition"
//  has a name: the binomial coefficient (thanks, Pascal!)
//  and is calculated as (n * (n+1))/2
function binomialCost(int[] crabs, int targetPosition) returns int {
    return int:sum(...(
        from int crab in crabs
        let int stepsToMove = int:abs(crab - targetPosition)
        select (stepsToMove * (stepsToMove + 1))/2
    ));
}

type CostFunction function(int[] crabs, int targetPosition) returns int;

function solve(CostFunction costFn, int[] crabs) returns int {
    int[] workingCopy = crabs.clone(); // so that Part A doesn't affect Part B
    int highestPosition = int:max(0, ...workingCopy);
    return int:min(int:SIGNED32_MAX_VALUE, ...(
        from int position in 0...highestPosition
        select costFn(workingCopy, position)
    ));
}

public function main() {
    string input = (checkpanic io:fileReadLines("input.txt"))[0];
    int[] crabs = 
        from string component in regex:split(input, ",")
        where component.trim().length() > 0
        select checkpanic int:fromString(component);
    
    io:println("Part A: ", solve(fixedCost, crabs));
    io:println("Part B: ", solve(binomialCost, crabs));
}
