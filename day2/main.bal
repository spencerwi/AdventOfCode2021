import ballerina/io;
import ballerina/regex;

type Command "forward"|"up"|"down";

type Instruction record {
    Command command;
    int amount;
};

type Point record {
    int x;
    int y;
};
type SubState record {
    int x;
    int y;
    int aim;
};

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

function move(Point current, Instruction instruction) returns Point {
    var {x, y} = current;
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
    return {x, y};

}

function moveWithAim(SubState current, Instruction instruction) returns SubState {
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

function partA(Instruction[] input) returns int {
    var {x, y} = input.reduce(move, {x: 0, y: 0});
    return x * y;
}

function partB(Instruction[] input) returns int {
    var {x, y} = input.reduce(moveWithAim, {x: 0, y: 0, aim: 0});
    return x * y;
}

public function main() returns error? {
    string[] lines = check io:fileReadLines("input.txt");

    Instruction[] input = 
        from string line in lines
        select parseInstruction(line);
    
    io:println("Part A: ", partA(input));
    io:println("Part B: ", partB(input));
}
