open System.Collections.Generic

module Grid = begin
    type t = int[,]
    type Point = int * int // row * column

    let findNeighbors grid ((row, column) as point) =
        let height = Array2D.length1 grid in
        let width = Array2D.length2 grid in
        let isInBounds (r, c) =
            r >= 0 && r < height &&
            c >= 0 && c < width
        in
        let possibleNeighborCoords = [
                            (row - 1, column); 
            (row, column - 1); (* start *) (row, column + 1);
                            (row + 1, column)
        ] in
        [
            for candidateNeighbor in possibleNeighborCoords do
                if isInBounds candidateNeighbor && candidateNeighbor <> point then
                    yield candidateNeighbor
        ]

    let isLowPoint value neighbors =
        neighbors
        |> Seq.forall (fun neighbor -> value < neighbor)

    let findBasinStartingWith (point : Point) (grid: t) : Set<Point> =
        // BFS up in this, yo
        let pointsToVisit = new Queue<Point>([|point|]) in
        let mutable pointsSeen = Set.empty in
        while pointsToVisit.Count > 0 do
            let (row, col) as currentPoint = pointsToVisit.Dequeue() in
            if not (pointsSeen.Contains currentPoint) then // we can't revisit points, because if we did, we'd infinitely loop
                let value = grid[row, col] in
                if value <> 9 then
                    pointsSeen <- Set.add currentPoint pointsSeen;
                    for neighbor in findNeighbors grid currentPoint do
                        pointsToVisit.Enqueue(neighbor)
        pointsSeen
end

let solve grid =
    let lowPointValues, lowPointCoords = List.unzip <| [
        for row in 0..((Array2D.length1 grid) - 1) do
            for column in 0..((Array2D.length2 grid - 1)) do
                let value = grid[row, column] in
                let neighbors = Grid.findNeighbors grid (row, column) in
                let neighborValues = neighbors |> Seq.map (fun (row, column) -> grid[row, column]) in
                if Grid.isLowPoint value neighborValues then
                    yield (value, (row, column))
    ]
    in
    let partA = 
        lowPointValues
        |> Seq.sumBy ((+) 1)

    // A list of the basins we've seen so far, so that we don't double-count
    let mutable basins = [] in
    let pointAlreadySeenInAnotherBasin point =
        basins
        |> List.exists (Set.contains point)
    for (row, column) in lowPointCoords do
        if not (pointAlreadySeenInAnotherBasin (row, column)) then
            basins <- (Grid.findBasinStartingWith (row, column) grid) :: basins
    let partB =
        basins
        |> Seq.map Set.count
        |> Seq.sortDescending 
        |> Seq.take 3
        |> Seq.fold (*) 1
    in (partA, partB)

[<EntryPoint>]
let main argv = 
    let filename = 
        if argv.Length > 0 then argv[0]
        else "input.txt"
    in
    let digitsInLine line = 
        Array.ofSeq line
        |> Array.map (string >> int)
    let grid = array2D [|
        for line in System.IO.File.ReadAllLines filename do
            yield digitsInLine line
    |]
    in
    let partA, partB = solve grid in
    printfn "Part A: %d" partA;
    printfn "Part B: %d" partB;
    0