import ballerina/io;

// Ballerina technically _has_ a Char type, but it doesn't work right with match
// statements, and doesn't appear to have a literal form. So we just use string
//  as char everywhere.

type ParseResult record {
    string? err;
    string[] unmatched;
};

function scoreError(string c) returns int {
    match c {
        ")" => { return 3; }
        "]" => { return 57; }
        "}" => { return 1197; }
        ">" => { return 25137; }
        _ => { return 0; }
    }
}

function scoreIncomplete(string[] remaining) returns int {
    var valueOf = function(string c) returns int {
        match c {
            ")" => { return 1; }
            "]" => { return 2; }
            "}" => { return 3; }
            ">" => { return 4; }
            _ => { return 0; }
        }
    };
    int total = 0;
    while remaining.length() > 0 {
        string next = remaining.pop();
        total = (total * 5) + valueOf(next);
    }
    return total;
}

function findError(string line) returns ParseResult {
    string[] expectedClosers = [];
    foreach int i in 0..<line.length() { 
        // Trying to do a for-each over the string itself yields a 
        //  "pattern will not be matched" warning on all string patterns.
        // So we do this instead.
        string c = line[i];
        match c {
            "(" => { expectedClosers.push(")"); }
            "[" => { expectedClosers.push("]"); }
            "{" => { expectedClosers.push("}"); }
            "<" => { expectedClosers.push(">"); }
            _ => { 
                string nextExpected = expectedClosers.pop();
                if c != nextExpected {
                    return {err: c, unmatched: expectedClosers};
                }
            }
        }
    }
    return {err: null, unmatched: expectedClosers};
}

function solve(string[] input) returns [int, int] {
    ParseResult[] results = input.map(findError);
    int partA = int:sum(...
        from var {err} in results
        where err != null
        select scoreError(err)
    );

    int[] incompleteScores = ( 
        from var {err, unmatched} in results
        where err == null
        let int score = scoreIncomplete(unmatched)
        order by score ascending
        select score
    );
    int partB = incompleteScores[incompleteScores.length() / 2];

    return [partA, partB];
}

public function main() {
    string[] input = checkpanic io:fileReadLines("input.txt");
    var [partA, partB] = solve(input);
    io:println("Part A: ", partA);
    io:println("Part B: ", partB);
}