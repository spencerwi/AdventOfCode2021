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

end

let partA = Maze.findSafestPath

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
    0
