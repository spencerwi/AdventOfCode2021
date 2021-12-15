open System
open System.Collections.Generic

module Maze = begin
    type t = int[,]
    type Point = {
        row: int
        col: int
    }

    type DistanceFromSource = {
        parent: Point option
        cost: int
    }

    let parse (inputLines: string[]) : t =
        array2D [|
            for line in inputLines do
                yield [|
                    for char in line do
                        yield (char |> string |> int)
                |]
        |]

    /// <returns>(height, width) of the maze</returns>
    let size maze =
        (Array2D.length1 maze, Array2D.length2 maze)
    
    let neighborsOf point maze =
        let height, width = size maze in
        let neighborCandidates = [
                                    {point with row = point.row - 1};
            {point with col = point.col - 1};           {point with col = point.col + 1};
                                    {point with row = point.row + 1};
        ] in
        query {
            for candidate in neighborCandidates do
            where (candidate.row >= 0 && candidate.row < height)
            where (candidate.col >= 0 && candidate.col < width)
            select candidate
        }

    let findSafestPath maze =
        let height, width = size maze in
        let goal = {row = height - 1; col = width - 1} in

        // Let's dijkstra this business, yo
        let distancesFromSource = Array2D.create height width {parent = None; cost = Int32.MaxValue} in
        distancesFromSource[0,0] <- {parent = None; cost = 0}
        let unsettled = new Queue<Point>([|{row = 0; col = 0}|]) in
        while unsettled.Count > 0 do
            let current = unsettled.Dequeue() in
            for neighbor in (neighborsOf current maze) do
                let weight = maze[neighbor.row, neighbor.col] in
                let existingDistance = distancesFromSource[neighbor.row, neighbor.col].cost in
                let distanceThroughCurrent = distancesFromSource[current.row, current.col].cost + weight in
                if distanceThroughCurrent < existingDistance then
                    distancesFromSource[neighbor.row, neighbor.col] <- {parent = Some current; cost = distanceThroughCurrent}
                    unsettled.Enqueue neighbor
        ;
        distancesFromSource[goal.row, goal.col].cost

    /// <summary>
    /// Expands the maze based on these rules:
    /// The entire cave is actually five times larger in both dimensions 
    /// than you thought;  the area you originally scanned is just one tile 
    /// in a 5x5 tile area that forms the full map. Your original map tile 
    /// repeats to the right and downward; each time the tile repeats to the
    /// right or downward, all of its risk levels are 1 higher than the tile
    /// immediately up or left of it. However, risk levels above 9 wrap back
    /// around to 1. So, if your original map had some position with a risk
    /// level of 8, then that same position on each of the 25 total tiles 
    /// would be as follows:
    ///  8 9 1 2 3
    ///  9 1 2 3 4
    ///  1 2 3 4 5
    ///  2 3 4 5 6
    ///  3 4 5 6 7
    /// </summary>
    let expand (maze : t) : t =
        let originalHeight, originalWidth = size maze in
        Array2D.init (originalHeight * 5) (originalWidth * 5) (fun row col ->
            let originalMazeValue = maze[row % originalHeight, col % originalWidth] in
            let tileDistanceAway = (row / originalHeight) + (col / originalWidth) in
            let newValue = originalMazeValue + tileDistanceAway in
            if newValue > 9 then
                (newValue % 9) 
            else newValue
        )

end

let partA = Maze.findSafestPath

let partB = Maze.expand >> Maze.findSafestPath

[<EntryPoint>]
let main argv =
    let filename =
        if argv.Length > 0 then argv[0]
        else "input.txt"
    in
    let input = 
        System.IO.File.ReadAllLines filename
        |> Maze.parse
    in
    printfn "Part A: %d" (partA input)
    printfn "Part B: %d" (partB input)
    0
