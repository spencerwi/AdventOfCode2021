import ballerina/io;
import ballerina/regex;

type RawLine record {
    string[] shown;
    string[] output;
};

function sortString(string input) returns string {
    string[] chars = 
        from var wire in input
        select wire;
    return "".join(...(chars.sort()));
}

function isDistinctWireCountDigit(string wires) returns boolean {
    match wires.length() {
        2|3|4|7 => { return true; }
        _ => { return false; }
    }
}

function buildMapping(string[] shownDigits) returns map<int> {
    map<int> mapping = {};
    var markAs = function(string digit, int value) {
        mapping[sortString(digit)] = value;
    };

    string[] uncertainDigits = [];

    // First, mark down the digits we can identify by how many unique wires
    // are lit.
    foreach string digit in shownDigits {
        match digit.length() {
            2 => { markAs(digit, 1); }
            3 => { markAs(digit, 7); }
            4 => { markAs(digit, 4); }
            7 => { markAs(digit, 8); }
            _ => { uncertainDigits.push(digit); }
        }
    }

    // Now we can start making deductions about the rest.
    var getDigitFor = function(int value) returns string {
        foreach var [k, v] in map:entries(mapping) {
            if v == value {
                return k;
            }
        }
        panic error("Value not found in mapping");
    };
    var hasAllWiresFrom = function(string digit, string fromDigit) returns boolean {
        foreach string wire in fromDigit {
            if !(digit.includes(wire)) {
                return false;
            }
        }
        return true;
    };
    var digitsOfLength = function(int n) returns string[] {
        return 
            from string digit in uncertainDigits
            where digit.length() == n
            select digit;
    };

    // We next need to determine which digit is 6. This is because we need to
    //  have identified 6 in order to identify 5 (and 2). Along the way, we'll
    //  wind up identifying 0 and 9.
    foreach string digit in digitsOfLength(6) {
        // This is either a 0, a 6, or a 9.
        // When 9 is lit up, all the lights from 4 are also lit up.
        // This is not true for 0 or 6.
        if (hasAllWiresFrom(digit, getDigitFor(4))) {
            markAs(digit, 9);
        } else if hasAllWiresFrom(digit, getDigitFor(1)) {
            // When 0 is lit up, all the wires from 1 are lit up. 
            // This is not true for 6.
            markAs(digit, 0);
        } else {
            markAs(digit, 6);
        }
    }

    // Now we can identify the remaining digits, all of which are of length 5.
    foreach string digit in digitsOfLength(5) {
        // This is either a 2, a 3, or a 5.
        // When 3 is lit up, all the lights from 1 are also lit up.
        // This is not true for 2 or 5.
        if (hasAllWiresFrom(digit, getDigitFor(1))) {
            markAs(digit, 3);
        } else if hasAllWiresFrom(getDigitFor(6), digit) {
            // When 6 is lit up, all the wires from 5 are also lit up.
            // The same is not true for 2.
            markAs(digit, 5);
        } else {
            markAs(digit, 2);
        }
    }

    return mapping;
}

function translate(map<int> mapping, string[] outputDigits) returns int {
    string outputStr = 
        from string digit in outputDigits
        let int? intValue = mapping[sortString(digit)]
        where intValue != null
        select string`${intValue}`;

    return checkpanic int:fromString(outputStr);
}

function partA(string[][] outputDigits) returns int {
    return (
        from string[] line in outputDigits 
        from string digit in line
        where isDistinctWireCountDigit(digit)
        select digit
    ).length();
}

function partB(RawLine[] lines) returns int {
    return int:sum(...(
        from var {shown, output} in lines
        let var mapping = buildMapping(shown)
        select translate(mapping, output)
    ));
}

public function main() {
    string[] inputLines = checkpanic io:fileReadLines("input.txt");
    RawLine[] lines = 
        from string line in inputLines
        let string[] segments = regex:split(line, " -> ")
        let string[] shown = regex:split(segments[0], "\\s+")
        let string[] output = regex:split(segments[1], "\\s+")
        select {shown, output};
    
    string[][] outputDigits = 
        from var {output} in lines
        select output;

    io:println("Part A: ", partA(outputDigits));
    io:println("Part B: ", partB(lines));
}
