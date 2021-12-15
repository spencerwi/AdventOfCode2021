open System
open System.Collections.Generic

module Maze = begin
    type t = int[,]
    type Point = {
        row: int
        col: int
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

    let findSafestPathRisk maze =
        let height, width = size maze in
        let goal = {row = height - 1; col = width - 1} in

        // Let's dijkstra this business, yo
        
        // We'll keep track of how far each point in the grid is from the source by using a separate grid of the same size
        let distancesFromSource = Array2D.create height width Int32.MaxValue in
        distancesFromSource[0,0] <- 0 // the source *is* the source, my dude

        // Now, we want to walk from the source "outwards" and trace each shortest path. 
        // Sometimes, we'll see a cell again but on a shorter path. No worries, dawg, just 
        // update its distance-from-the-source and go check it out again to see if anything's 
        // changed about it.
        let unsettled = new Queue<Point>([|{row = 0; col = 0}|]) in
        while unsettled.Count > 0 do
            let current = unsettled.Dequeue() in
            for neighbor in (neighborsOf current maze) do
                let weight = maze[neighbor.row, neighbor.col] in
                let existingDistance = distancesFromSource[neighbor.row, neighbor.col] in
                let distanceThroughCurrent = distancesFromSource[current.row, current.col] + weight in
                if distanceThroughCurrent < existingDistance then
                    distancesFromSource[neighbor.row, neighbor.col] <- distanceThroughCurrent
                    unsettled.Enqueue neighbor
        ;

        // Finally, we can answer the question: 
        // how risky (far) is the safest (shortest) path from the top-left to 
        // the bottom-right?
        distancesFromSource[goal.row, goal.col]

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

let partA = Maze.findSafestPathRisk

let partB = Maze.expand >> Maze.findSafestPathRisk

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
