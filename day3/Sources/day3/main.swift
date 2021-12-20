import Foundation

// To allow the more-convenient "foo"[1] sort of thing
extension StringProtocol {
    subscript(offset: Int) -> Character {
        self[index(startIndex, offsetBy: offset)]
    }
}

enum MostOrLeast {
    case most
    case least
}

func flipBit(_ bit: Character) -> Character {
    switch bit {
        case "0": return "1"
        case "1": return "0"
        case _: fatalError("Invalid bit: \(bit)")
    }
}

func mostCommonBitInColumn<T>(column: Int, _ inputs: [T]) -> Character where T : StringProtocol {
    let totalNumberOfInputs = inputs.count
    let onesInThisColumn = inputs.filter({$0[column] == "1"}).count
    let zeroesInThisColumn = totalNumberOfInputs - onesInThisColumn
    if onesInThisColumn >= zeroesInThisColumn {
        return "1"
    } else {
        return "0"
    }
}

func mostAndLeastCommonBits<T>(_ inputs: [T]) -> (String, String) where T : StringProtocol {
    let bitsPerLine = inputs[0].count
    let mostCommonBits = String((0..<bitsPerLine).map({ mostCommonBitInColumn(column: $0, inputs) }))
    let leastCommonBits = String((0..<bitsPerLine).map({ flipBit(mostCommonBits[$0]) }))
    return (mostCommonBits, leastCommonBits)
}

func gammaAndEpsilon<T>(_ inputs: [T]) -> (Int, Int) where T : StringProtocol {
    let (mostCommonBits, leastCommonBits) = mostAndLeastCommonBits(inputs)
    return (
        Int(mostCommonBits, radix:2)!,
        Int(leastCommonBits, radix:2)!
    )
}

func powerConsumption<T>(_ inputs: [T]) -> Int where T : StringProtocol {
    let (gamma, epsilon): (Int, Int) = gammaAndEpsilon(inputs)
    return gamma * epsilon
}

func filterByBitCriteria<T>(commonality: MostOrLeast, startColumn: Int, _ rows: [T]) -> Int where T : StringProtocol {
    if rows.count == 1 {
        return Int(rows[0], radix: 2)!;
    }

    let mostCommonBit = mostCommonBitInColumn(column: startColumn, rows)
    let bitToMatch : Character
    switch commonality {
        case .most: bitToMatch = mostCommonBit
        case .least: bitToMatch = flipBit(mostCommonBit)
    }
    let matches = rows.filter { $0[startColumn] == bitToMatch }
    return filterByBitCriteria(commonality: commonality, startColumn: startColumn + 1, matches)
}

func lifeSupportRating<T>(_ inputs: [T]) -> Int where T : StringProtocol {
    let oxygenGeneratorRating = filterByBitCriteria(commonality: MostOrLeast.most, startColumn: 0, inputs)
    let co2ScrubberRating = filterByBitCriteria(commonality: MostOrLeast.least, startColumn: 0, inputs)
    return oxygenGeneratorRating * co2ScrubberRating
}

let lines = try! String(contentsOfFile: "input.txt").split(separator: "\n")
let partA = powerConsumption(lines)
print("Part A: \(partA)")
let partB = lifeSupportRating(lines)
print("Part B: \(partB)")