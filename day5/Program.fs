open System.Text.RegularExpressions

let minAndMax a b =
    if a > b then
        b, a
    else
        a, b

module Line = begin
    type Point = {
        x: int
        y: int
    }
    type t = {
        start: Point
        stop: Point
    }

    let parse inputStr = 
        let pattern = "(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)" in
        let matched = Regex.Match(inputStr, pattern) in
        match matched.Groups["x1"].Value, matched.Groups["y1"].Value, matched.Groups["x2"].Value, matched.Groups["y2"].Value with
        | x1s, y1s, x2s, y2s ->
            { start = {x = int x1s; y = int y1s}; stop = {x = int x2s; y = int y2s}} 
        | _ -> failwith "Did not match!"

    let isHorizontal line =
        line.start.y = line.stop.y

    let isVertical line =
        line.start.x = line.stop.x
end

module Grid = begin
    type t = int[,]

    type SizeSpec = {
        height: int
        width: int
    }

    let drawLine (grid : t) (line : Line.t) =
        let xIncrement =
            if line.start.x < line.stop.x then 1
            elif line.start.x > line.stop.x then -1
            else 0
        let yIncrement = 
            if line.start.y < line.stop.y then 1
            elif line.start.y > line.stop.y then -1
            else 0
        let mutable x = line.start.x in
        let mutable y = line.start.y in
        while (x <> line.stop.x + xIncrement) || (y <> line.stop.y + yIncrement) do
            grid[x,y] <- grid[x,y] + 1
            x <- x + xIncrement
            y <- y + yIncrement 

    let countOverlaps grid =
        seq {
            for row in 0..((Array2D.length1 grid) - 1) do
                for col in 0..((Array2D.length2 grid) - 1) do
                    if grid[row, col] > 1 then
                        yield grid[row, col]
        } |> Seq.length
end

let solve (gridSize : Grid.SizeSpec) lines =
    let grid = Array2D.create gridSize.width gridSize.height 0 in

    // Part A: draw all the non-diagonal lines, and count the overlaps
    for line in lines do
        if Line.isHorizontal line || Line.isVertical line then
            Grid.drawLine grid line
    let partA = Grid.countOverlaps grid in

    // Part B: now draw all the diagonal lines too, and count the overlaps again
    for line in lines do
        if not (Line.isHorizontal line) && not (Line.isVertical line) then
            Grid.drawLine grid line
    (partA, Grid.countOverlaps grid)
    

[<EntryPoint>]
let main argv =
    let inputLines = 
        System.IO.File.ReadAllLines "input.txt" 
        |> Seq.map Line.parse

    let gridWidth =
        inputLines
        |> Seq.map (fun line -> max line.start.x line.stop.x)
        |> Seq.max 
        |> ((+) 1) // we need one more space than the largest X seen to accommodate 0 through largestX

    let gridHeight = 
        inputLines
        |> Seq.map (fun line -> max line.start.y line.stop.y)
        |> Seq.max 
        |> ((+) 1) // we need one more space than the largest Y seen to accommodate 0 through largestY

    let partA, partB = solve {width=gridWidth; height=gridHeight} inputLines in
    printfn "Part A: %d" partA
    printfn "Part B: %d" partB
    0