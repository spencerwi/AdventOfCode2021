import ballerina/io;
import ballerina/regex;

type Command "forward"|"up"|"down";

type Instruction record {
    Command command;
    int amount;
};

type State record {
    int x;
    int y;
    int aim;
};
State initialState = {x: 0, y: 0, aim: 0};

type MoverFunction function (State, Instruction) returns State;

function parseInstruction(string line) returns Instruction {
    if !line.startsWith("forward") && !line.startsWith("up") && !line.startsWith("down") {
        panic error("Invalid command: " + line);
    }

    // Ballerina doesn't yet have great regex support libs, so we're stuck with doing string-splitting here
    string[] components = regex:split(line, " ");
    Command command = <Command>components[0];
    int amount = checkpanic int:fromString(components[1]);
    return {command, amount};
}

function move(State current, Instruction instruction) returns State {
    var {x, y, aim} = current;
    match instruction.command {
        "forward" => { 
            x += instruction.amount;
        }
        "up" => {
            y -= instruction.amount; // We increase y as we go DOWN for this puzzle
        }
        "down" => {
            y += instruction.amount; 
        }
    }
    return {x, y, aim};

}

function moveWithAim(State current, Instruction instruction) returns State {
    var {x, y, aim} = current;
    match instruction.command {
        "forward" => { 
            x += instruction.amount;
            y += (aim * instruction.amount);
        }
        "up" => {
            aim -= instruction.amount;
        }
        "down" => {
            aim += instruction.amount;
        }
    }
    return {x, y, aim};
}

function solve(MoverFunction mover, Instruction[] input) returns int {
    var {x, y} = input.reduce(mover, initialState);
    return x * y;
}

function partA(Instruction[] input) returns int {
    return solve(move, input);
}

function partB(Instruction[] input) returns int {
    return solve(moveWithAim, input);
}

public function main() returns error? {
    string[] lines = check io:fileReadLines("input.txt");

    Instruction[] input = 
        from string line in lines
        select parseInstruction(line);
    
    io:println("Part A: ", partA(input));
    io:println("Part B: ", partB(input));
}
