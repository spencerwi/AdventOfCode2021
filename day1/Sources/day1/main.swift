import Foundation

func countIncreases(_ input: [Int]) -> Int {
    let increases: [Int] = (1..<input.count)
        .filter({i in 
            let current = input[i]
            let previous = input[i - 1]
            return current > previous
        })
    return increases.count
}

func makeSlidingSumList(_ input: [Int]) -> [Int] {
    return (2..<input.count).map {(i: Int) in input[i - 2] + input[i - 1] + input[i]}
}

func solve(_ input: [Int]) -> (Int, Int) {
    return (countIncreases(input), countIncreases(makeSlidingSumList(input)));
}

let lines = try! String(contentsOfFile: "input.txt").split(separator: "\r\n")
let ints = lines.map {line in Int(line)}.filter({$0 != nil}).map({$0!})
let (partA, partB): (Int, Int) = solve(ints)
print("Part A: \(partA)")
print("Part B: \(partB)")