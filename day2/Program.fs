open System.Text.RegularExpressions

type Point = {x: int; y: int}
type SubStatus = {x: int; y: int; aim: int}

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

    let move (current: Point) = function
        | Forward x -> {current with x = current.x + x}
        | Down y -> {current with y = current.y + y} // We increase our "y" as we go *down* for this puzzle
        | Up y -> {current with y = current.y - y} // We increase our "y" as we go *down* for this puzzle

    let move_with_aim (current: SubStatus) = function 
        | Forward x -> {current with x = current.x + x; y = current.y + (current.aim * x)}
        | Down y -> {current with aim = current.aim + y}
        | Up y -> {current with aim = current.aim - y}
end

let part_a (input: Instruction.t seq) = 
    let final = Seq.fold Instruction.move {x=0; y=0} input 
    in final.x * final.y

let part_b (input: Instruction.t seq) =
    let final = Seq.fold Instruction.move_with_aim {x=0; y=0; aim=0} input 
    in final.x * final.y

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