open System.Collections.Generic

module Grid = begin
    type t = int[,]
    type Point = {row: int; column: int}

    let parse (inputLines : string[]) =
        array2D <| [| 
            for line in inputLines do
                yield 
                    line
                    |> Seq.map (string >> int)
                    |> Array.ofSeq
        |]

    let pointsIn grid = 
        let height = Array2D.length1 grid in
        let width = Array2D.length2 grid in
        seq {
            for row in 0..(height - 1) do
            for column in 0..(width - 1) do
            yield {row = row; column = column}
        }

    let isFlash (grid : t) point =
        grid[point.row, point.column] > 9

    let getNeighbors point grid =
        let isInBounds row col = 
            row >= 0 && row < Array2D.length1 grid &&
            col >= 0 && col < Array2D.length2 grid
        query {
            for row in (point.row - 1)..(point.row + 1) do
            for col in (point.column - 1)..(point.column + 1) do
            let neighbor = {row = row; column = col}
            where ((isInBounds row col) && (neighbor <> point))
            select neighbor
        }

    let increment (grid : t) point =
        let value = grid[point.row, point.column] in
        grid[point.row, point.column] <- value + 1


    let step grid =
        // First, increment everything by 1. As we do, look for any cells that are going
        //  to flash, and enqueue them for processing.
        let flashesToProcess = new Queue<Point>() in
        for point in pointsIn grid do
            increment grid point
            if isFlash grid point then // it's flashing
                flashesToProcess.Enqueue point

        // Now process all the flashing cells by incrementing all their (unflashed) neighbors.
        // Then, once we've processed all those flashes, scan for flashes again, and
        // process those. We have to keep doing this until things "stabilize"
        let mutable flashedCells = Set.empty
        while flashesToProcess.Count > 0 do
            for _ in 0..(flashesToProcess.Count - 1) do
                let currentFlash = flashesToProcess.Dequeue() in
                getNeighbors currentFlash grid
                |> Seq.filter (fun neighbor -> not (Set.contains neighbor flashedCells)) // if a cell flashes, don't increment it -- flashing locks it at 0 until the next step
                |> Seq.iter (increment grid);

                grid[currentFlash.row, currentFlash.column] <- 0;
                flashedCells <- Set.add currentFlash flashedCells

            // Now look for the new flashes and process those
            pointsIn grid 
            |> Seq.filter (isFlash grid)
            |> Seq.iter flashesToProcess.Enqueue

        Set.count flashedCells

    let size grid =
        (Array2D.length1 grid) * (Array2D.length2 grid)
end

let solve input =
    let grid = Grid.parse input in
    let mutable stepCounter = 0 in
    let mutable totalFlashCount = 0
    let mutable everyoneFlashed = false
    while not everyoneFlashed do
        stepCounter <- stepCounter + 1
        let flashCountThisStep = Grid.step grid in
        totalFlashCount <- totalFlashCount + flashCountThisStep
        if stepCounter = 100 then
            printfn "Part A: %d" totalFlashCount
        everyoneFlashed <- (flashCountThisStep = Grid.size grid)

    printfn "Part B: %d" stepCounter


[<EntryPoint>]
let main argv =
    let filename = 
        if argv.Length > 0 then argv[0]
        else "input.txt"
    in
    let input = System.IO.File.ReadAllLines filename in
    solve input
    0