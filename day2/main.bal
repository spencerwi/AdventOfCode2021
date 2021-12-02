import ballerina/io;
import ballerina/regex;

type Instruction record {
    "forward"|"up"|"down" command;
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
    int amount = checkpanic int:fromString(components[1]);
    match components[0] {
        "forward" => { 
            return {command: "forward", amount: amount}; 
        }
        "up" => { 
            return {command: "up", amount: amount}; 
        }
        "down" => { 
            return {command: "down", amount: amount}; 
        }
        _ => { panic error("This should not be reachable!"); }
    }
}

function move(Point current, Instruction instruction) returns Point {
    match instruction.command {
        "forward" => { 
            return {x: current.x + instruction.amount, y: current.y}; 
        }
        "up" => {
            return {x: current.x, y: current.y - instruction.amount}; // We increase y as we go DOWN for this puzzle
        }
        "down" => {
            return {x: current.x, y: current.y + instruction.amount};
        }
        _ => { panic error("This should not be reachable!"); }
    }

}

function moveWithAim(SubState current, Instruction instruction) returns SubState {
    match instruction.command {
        "forward" => { 
            return {x: current.x + instruction.amount, y: current.y + (current.aim * instruction.amount), aim: current.aim}; 
        }
        "up" => {
            return {x: current.x, y: current.y, aim: current.aim - instruction.amount};
        }
        "down" => {
            return {x: current.x, y: current.y, aim: current.aim + instruction.amount};
        }
        _ => { panic error("This should not be reachable!"); }
    }
}

function partA(Instruction[] input) returns int {
    Point finalPosition = input.reduce(move, {x: 0, y: 0});
    return finalPosition.x * finalPosition.y;
}

function partB(Instruction[] input) returns int {
    SubState finalPosition = input.reduce(moveWithAim, {x: 0, y: 0, aim: 0});
    return finalPosition.x * finalPosition.y;
}

public function main() returns error? {
    string[] lines = check io:fileReadLines("input.txt");

    Instruction[] input = 
        from string line in lines
        select parseInstruction(line);
    
    io:println("Part A: ", partA(input));
    io:println("Part B: ", partB(input));
}
