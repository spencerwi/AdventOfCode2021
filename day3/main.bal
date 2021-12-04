import ballerina/io;

type MostOrLeast "most"|"least"; 

// Surprisingly, Ballerina doesn't have an int exponentiation operator.
function intpow(int base, int exponent) returns int {
    if (exponent == 0) { return 1; }
    int result = base;
    foreach int i in 1..<exponent {
        result *= base;
    }
    return result;
}

// Ballerina also doesn't have a built-in conversion from binary string to int.
function bitStringToInt(string bitstring) returns int {
    int result = 0;
    foreach int i in 0..<bitstring.length() {
        if bitstring[i] == "1" {
            result += intpow(2, ((bitstring.length() - i) - 1));
        }
    }
    return result;
}

function flipBit(string bit) returns string {
    match bit {
        "0" => { return "1"; }
        "1" => { return "0"; }
        _ => { panic error("Unrecognized bit: " + bit); }
    }
}

function mostCommonBitInColumn(int column, string[] inputs) returns string {
    string[] rowsWithOneInColumn =
            from var row in inputs
    where row[column] == "1"
    select row;
    int onesInColumn = rowsWithOneInColumn.length();
    int zeroesInColumn = inputs.length() - onesInColumn;
    if onesInColumn >= zeroesInColumn {
        return "1";
    } else {
        return "0";
    }
}

function mostAndLeastCommonBits(string[] inputs) returns [string, string] {
    string mostCommonBits = 
        from int column in 0..<inputs[0].length()
        select mostCommonBitInColumn(column, inputs);
    string leastCommonBits = 
        from var mostCommonBit in mostCommonBits
        select flipBit(mostCommonBit);

    return [mostCommonBits, leastCommonBits];
}

function gammaAndEpsilon(string[] inputs) returns [int, int] {
    var [mostCommonBits, leastCommonBits] = mostAndLeastCommonBits(inputs);
    int gamma = bitStringToInt(mostCommonBits);
    int epsilon = bitStringToInt(leastCommonBits) ;
    return [gamma, epsilon];
}

function powerConsumption(string[] inputs) returns int {
    var [gamma, epsilon] = gammaAndEpsilon(inputs);
    return gamma * epsilon;
}

function filterByBitCriteria(MostOrLeast mostOrLeastCommon, string[] inputs) returns int {
    string[] matchingRows = inputs;
    int columnBeingChecked = 0;
    while (matchingRows.length() != 1) {
        string mostCommonBit = mostCommonBitInColumn(columnBeingChecked, matchingRows);
        string bitToMatch = "";
        match mostOrLeastCommon {
            "most" => { bitToMatch = mostCommonBit; }
            "least" => { bitToMatch = flipBit(mostCommonBit); }
        }
        matchingRows = 
            from string row in matchingRows
            where row[columnBeingChecked] == bitToMatch
            select row;

        columnBeingChecked += 1;
    }
    return bitStringToInt(matchingRows[0]);
}

function lifeSupportRating(string[] inputs) returns int {
    int oxygenGeneratorRating = filterByBitCriteria("most", inputs);
    int co2ScrubberRating = filterByBitCriteria("least", inputs);
    return oxygenGeneratorRating * co2ScrubberRating;
}

public function main() returns error? {
    string[] lines = check io:fileReadLines("input.txt");

    io:println("Part A: ", powerConsumption(lines));
    io:println("Part B: ", lifeSupportRating(lines));
}
