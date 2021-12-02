open System.Text.RegularExpressions

type Point = int * int // (x, y)
type SubStatus = Point * int //(position, aim)

module Instruction = begin
    type t = 
        Forward of int
        | Down of int
        | Up of int

    let parse (line: string) =
        let pattern = "(?<command>forward|up|down) (?<amount>\d+)"
        let matched = Regex.Match(line, pattern)
        match matched.Groups["command"].Value, matched.Groups["amount"].Value with
        | "forward", amount -> Forward (int amount)
        | "up", amount -> Up (int amount)
        | "down", amount -> Down (int amount)
        | other, _ -> failwith ("Unrecognized command: " + other)

    let move ((current_x, current_y): Point) = function
        | Forward x -> (current_x + x, current_y)
        | Down y -> (current_x, current_y + y) // We increase our "y" as we go *down* for this puzzle
        | Up y -> (current_x, current_y - y) // We increase our "y" as we go *down* for this puzzle

    let move_with_aim (((current_x, current_y), current_aim): SubStatus) = function 
        | Forward x -> ((current_x + x, current_y + (current_aim * x)), current_aim)
        | Down y -> ((current_x, current_y), current_aim + y)
        | Up y -> ((current_x, current_y), current_aim - y)
end

let part_a (input: Instruction.t seq) = 
    let final_x, final_y = Seq.fold Instruction.move (0,0) input
    in final_x * final_y

let part_b (input: Instruction.t seq) =
    let (final_x, final_y), _ = Seq.fold Instruction.move_with_aim ((0,0), 0) input 
    in final_x * final_y

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