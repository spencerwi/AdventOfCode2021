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
        let startX, stopX = minAndMax line.start.x line.stop.x in
        let startY, stopY = minAndMax line.start.y line.stop.y in 
        if Line.isHorizontal line then
            for x in startX..stopX do
                grid[x,startY] <- grid[x,startY] + 1
        elif Line.isVertical line then
            for y in startY..stopY do
                grid[startX,y] <- grid[startX,y] + 1
        else
            let xIncrement =
                if line.start.x < line.stop.x 
                then 1
                else -1
            let yIncrement = 
                if line.start.y < line.stop.y
                then 1
                else -1
            let mutable y = line.start.y in
            for x in (line.start.x)..xIncrement..(line.stop.x) do
                grid[x,y] <- grid[x,y] + 1
                y <- y + yIncrement 

    let draw gridSize lines =
        let grid = Array2D.create gridSize.width gridSize.height 0 in
        for line in lines do
            drawLine grid line
        grid

    let findOverlaps grid =
        seq {
            for row in 0..((Array2D.length1 grid) - 1) do
                for col in 0..((Array2D.length2 grid) - 1) do
                    if grid[row, col] > 1 then
                        yield grid[row, col]
        } |> Seq.length
end

let part_a gridSize lines =
    let isNotDiagonal line = 
        (Line.isHorizontal line) || (Line.isVertical line)
    let grid = Grid.draw gridSize (Seq.filter isNotDiagonal lines) in
    Grid.findOverlaps grid

let part_b gridSize lines =
    let grid = Grid.draw gridSize lines in
    Grid.findOverlaps grid

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

    printfn "Part A: %d" (part_a {width=gridWidth; height=gridHeight} inputLines)
    printfn "Part B: %d" (part_b {width=gridWidth; height=gridHeight} inputLines)
    0