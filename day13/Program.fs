open System.Text.RegularExpressions

let int_of_char c =
    c |> string |> int

module Folding = begin
    type Point = {
        x: int
        y: int
    }
    type Instruction = 
        FoldAlongY of y:int
        | FoldAlongX of x:int

    let parse (inputLines: string[]) = 
        let parsePoint (line : string) =
            let [|xStr;yStr|] = line.Split(",") in
            {x = int_of_char xStr; y = int_of_char yStr}
        let parseInstruction line =
            let pattern = "fold along (?<axis>x|y)=(?<value>\d+)" in
            let matched = Regex.Match(line, pattern) in
            match matched.Groups["axis"].Value, matched.Groups["value"].Value with 
            | "x", value -> FoldAlongX (int value)
            | "y", value -> FoldAlongY (int value)
        in
        let points =
            inputLines
            |> Seq.takeWhile (fun s -> s.Trim().Length > 0 && not (s.StartsWith "fold along"))
            |> Seq.map parsePoint
            |> Set.ofSeq
        in
        let instructions =
            inputLines
            |> Array.skipWhile (fun s -> not (s.StartsWith "fold along"))
            |> Array.map parseInstruction
        in
        (points, instructions)


    let rebaseToZero (instruction : Instruction) (dots : Set<Point>) =
        match instruction with
        | FoldAlongX _ -> 
            let xBase = 
                dots 
                |> Seq.map (fun dot -> dot.x)
                |> Seq.min
            in
            dots 
            |> Seq.map (fun dot -> {dot with x = dot.x - xBase})
            |> Set.ofSeq
        | FoldAlongY _ ->
            let yBase =
                dots
                |> Seq.map (fun dot -> dot.y)
                |> Seq.min
            dots 
            |> Seq.map (fun dot -> {dot with y = dot.y - yBase})
            |> Set.ofSeq


    let applyFold (dots : Set<Point>) (instruction : Instruction) =
        match instruction with
        | FoldAlongX foldX -> 
            seq { 
                for dot in dots do
                    if dot.x < foldX then
                        yield dot
                    elif dot.x > foldX then
                        let distanceToFoldLine = dot.x - foldX in
                        let newX = (dot.x - (distanceToFoldLine * 2)) in
                        yield {dot with x = newX}
            }
        | FoldAlongY foldY -> 
            seq {
                for dot in dots do
                    if dot.y < foldY then
                        yield dot
                    elif dot.y > foldY then
                        let distanceToFoldLine = dot.y - foldY in
                        let newY = (dot.y - (distanceToFoldLine * 2)) in
                        yield {dot with y = newY}
            }
        |> Set.ofSeq
        |> rebaseToZero instruction

    let printGrid dots =
        let gridHeight =
            dots
            |> Seq.map (fun dot -> dot.y)
            |> Seq.max
        let gridWidth =
            dots 
            |> Seq.map (fun dot -> dot.x)
            |> Seq.max
        in
        for row in 0..gridHeight do
            for col in 0..gridWidth do
                if Set.contains {y = row; x = col} dots then
                    printf "#"
                else
                    printf " "
            printf "\n"
end

[<EntryPoint>]
let main argv =
    let filename = 
        if argv.Length > 0 then argv[0]
        else "input.txt"
    in
    let (dots, instructions) =
        System.IO.File.ReadAllLines filename
        |> Folding.parse
    
    let partA = 
        Folding.applyFold dots instructions[0]
        // |> (fun dots -> Folding.printGrid dots; dots)
        |> Seq.length
    in
    let partB =
        instructions
        |> Array.fold Folding.applyFold dots
    printfn "Part A: %d" partA
    printfn "Part B:"
    Folding.printGrid partB
    0