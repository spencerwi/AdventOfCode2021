open System.Text.RegularExpressions

type State = {x: int; y: int; aim: int}

let initialState = {x=0; y=0; aim=0}

module Instruction = begin
    type t = 
        | Forward of int
        | Down of int
        | Up of int

    let parse (line: string) =
        let pattern = "(?<command>forward|up|down) (?<amount>\d+)"
        let matched = Regex.Match(line, pattern)
        let amount = int matched.Groups["amount"].Value 
        match matched.Groups["command"].Value with
        | "forward" -> Forward amount
        | "up" -> Up amount
        | "down" -> Down amount
        | other -> failwith ("Unrecognized command: " + other)

    let move (current: State) = function
        | Forward x -> {current with x = current.x + x}
        | Down y -> {current with y = current.y + y} // We increase our "y" as we go *down* for this puzzle
        | Up y -> {current with y = current.y - y} // We increase our "y" as we go *down* for this puzzle

    let move_with_aim (current: State) = function 
        | Forward x -> {current with x = current.x + x; y = current.y + (current.aim * x)}
        | Down y -> {current with aim = current.aim + y}
        | Up y -> {current with aim = current.aim - y}
end

let solve mover (input: Instruction.t seq) =
    let final = Seq.fold mover initialState input 
    in final.x * final.y


let part_a = solve Instruction.move 

let part_b = solve Instruction.move_with_aim 

[<EntryPoint>]
let main argv =
    let input = 
        System.IO.File.ReadAllLines "input.txt" 
        |> Seq.ofArray
        |> Seq.map Instruction.parse
    in
    printfn "Part A: %d" (part_a input);
    printfn "Part B: %d" (part_b input);
    0