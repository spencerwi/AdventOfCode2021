import Foundation

struct State {
    let x: Int
    let y: Int
    let aim: Int
}
let initialState = State(x:0, y:0, aim:0)

enum Instruction {
    case forward(Int)
    case up(Int)
    case down(Int)

    static func parse(from inputString: Substring) -> Instruction {
        let components = inputString.split(separator: " ")
        let amount = Int(components[1])!
        switch components[0] {
            case "forward": return forward(amount)
            case "up": return up(amount)
            case "down": return down(amount)
            case _: fatalError("Invalid string: \(inputString)")
        }
    }

    func move(_ state: State) -> State {
        switch self {
            case .forward(let amount):
                return State(x: state.x + amount, y: state.y, aim: state.aim)
            case .down(let amount):
                return State(x: state.x, y: state.y + amount, aim: state.aim)
            case .up(let amount):
                return State(x: state.x, y: state.y - amount, aim: state.aim)
        }
    }

    func moveWithAim(_ state: State) -> State {
        switch self {
            case .forward(let amount):
                return State(x: state.x + amount, y: (state.y + (state.aim * amount)), aim: state.aim)
            case .down(let amount):
                return State(x: state.x, y: state.y, aim: state.aim + amount)
            case .up(let amount):
                return State(x: state.x, y: state.y, aim: state.aim - amount)
        }
    }
}

func solve(_ inputLines: [Substring]) -> (Int, Int) {
    let instructions: [Instruction] = inputLines.map {Instruction.parse(from: $0)}
    let partAFinal = instructions.reduce(initialState, {(current: State, instruction: Instruction) in instruction.move(current)})
    let partBFinal = instructions.reduce(initialState, {(current: State, instruction: Instruction) in instruction.moveWithAim(current)})
    let partA = partAFinal.x * partAFinal.y
    let partB = partBFinal.x * partBFinal.y
    return (partA, partB)
}

let lines = try! String(contentsOfFile: "input.txt").split(separator: "\r\n")
let (partA, partB) = solve(lines)
print("Part A: \(partA)")
print("Part B: \(partB)")