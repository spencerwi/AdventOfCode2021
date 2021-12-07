import ballerina/io;
import ballerina/regex;

type FishBuckets int[];
function parseInput(string input) returns FishBuckets {
    int[] fishes = 
        from string component in regex:split(input, ",")
        where component.trim().length() > 0
        select checkpanic int:fromString(component);

    FishBuckets buckets = [0,0,0,0,0,0,0,0,0]; // 9 buckets: 0-8

    foreach int fish in fishes {
        buckets[fish] += 1;
    }
    return buckets;
}

function step(FishBuckets buckets) {
    int fishToAdd = buckets.shift();
    buckets.push(fishToAdd);
    buckets[6] += fishToAdd;
}

public function main() {
    string line = (checkpanic io:fileReadLines("input.txt"))[0];
    FishBuckets buckets = parseInput(line);
    foreach int x in 0..<256 {
        step(buckets);
        if (x == 79) {
            io:println("Part A: ", int:sum(...buckets));
        }
    }
    io:println("Part B: ", int:sum(...buckets));
}
