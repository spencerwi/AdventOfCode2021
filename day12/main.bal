import ballerina/io;
import ballerina/regex;

function isLowercase(string s) returns boolean {
    if s.toLowerAscii() == s {
        return true;
    } else {
        return false;
    }
}

function getOrDefault(map<string[]> m, string key, string[] default) returns string[] {
    if m.hasKey(key) {
        return <string[]>m[key];
    } else {
        return default;
    }
}

type CaveMap map<string[]>;
type TraversalStrategy record {
    function (string[], string) returns boolean canVisit;
    function (string[], string) returns string[] updateCaves;
};

TraversalStrategy partAStrategy = {
    canVisit: function(string[] caves, string cave) returns boolean {
        if (isLowercase(cave)) {
            return (caves.indexOf(cave) == null);
        } 
        return true;
    },
    updateCaves: function (string[] caves, string cave) returns string[] {
        string[] updatedCaves = caves.clone();
        if (!isLowercase(cave)) { return updatedCaves; }
        if (caves.indexOf(cave) == null) {
            updatedCaves.push(cave);
        }
        return updatedCaves;
    }
};

TraversalStrategy partBStrategy = {
    canVisit: function (string[] caves, string cave) returns boolean {
        if cave == "start" {
            return false;
        } 
        if !isLowercase(cave) { 
            return true;
        }
        if caves.indexOf(cave) == null {
            return true;
        }
        string[] revisitedSmallCaves = 
            from string c in caves
            where c.endsWith("2")
            select c;
        if revisitedSmallCaves.length() == 0 {
            return true;
        } 
        return (caves.indexOf(cave + "2") == null);
    },
    updateCaves: function (string[] caves, string cave) returns string[] {
        string[] updatedCaves = caves.clone();
        if (!isLowercase(cave)) { return updatedCaves; }
        if (caves.indexOf(cave) == null) {
            caves.push(cave);
            return updatedCaves;
        }
        string alias = cave + "2";
        if (caves.indexOf(alias) == null) {
            updatedCaves.push(alias);
            return updatedCaves;
        }
        panic error("Somehow visited a cave more than twice!");
    }
};

function buildCaveMap(string[] input) returns CaveMap {
    map<string[]> caveMap = {};
    foreach string line in input {
        var segments = regex:split(line, "-");
        var aDestinations = getOrDefault(caveMap, segments[0], []);
        var bDestinations = getOrDefault(caveMap, segments[1], []);
        if (aDestinations.indexOf(segments[1]) == null) {
            aDestinations.push(segments[1]);
        }
        if (bDestinations.indexOf(segments[0]) == null) {
            bDestinations.push(segments[0]);
        }
        caveMap[segments[0]] = aDestinations;
        caveMap[segments[1]] = bDestinations;
    }
    return caveMap;
}

// This hits a StackOverflowError in Ballerina for part 2 :(
// Need to find a different approach that Ballerina can handle.
function countPaths(TraversalStrategy strategy, CaveMap caveMap, string[] cavesAlreadyVisited, string current, string goal) returns int {
    if current == goal { return 1; }
    if !(caveMap.hasKey(current)) { return 0; }
    string[] neighbors = <string[]>caveMap[current];
    return int:sum(...
        from string neighbor in neighbors
        where strategy.canVisit(cavesAlreadyVisited, neighbor)
        let string[] updatedCaves = strategy.updateCaves(cavesAlreadyVisited, neighbor)
        select countPaths(strategy, caveMap, updatedCaves, neighbor, goal)
    );
}

function partA(CaveMap caveMap) returns int {
    return countPaths(partAStrategy, caveMap, ["start"], "start", "end");
}
function partB(CaveMap caveMap) returns int {
    return countPaths(partBStrategy, caveMap, ["start"], "start", "end");
}

public function main() {
    string[] lines = checkpanic io:fileReadLines("input.txt");
    CaveMap caveMap = buildCaveMap(lines);

    io:println("Part A: ", partA(caveMap));
    io:println("Part B: ", partB(caveMap));
}
