open System.Collections.Generic

type Point = int * int // row * column

let findNeighbors grid (row, column) =
    let height = Array2D.length1 grid in
    let width = Array2D.length2 grid in
    let possibleNeighborCoords = [
                        (column, row - 1); 
        (column - 1, row); (* start *) (column + 1, row);
                        (column, row + 1)
    ] in
    [
        for (neighborColumn, neighborRow) in possibleNeighborCoords do
            if neighborColumn >= 0 && neighborColumn < width then
                if neighborRow >= 0 && neighborRow < height then
                    yield neighborRow, neighborColumn
    ]

let isLowPoint value neighbors =
    neighbors
    |> Seq.forall (fun neighbor -> value < neighbor)

let findBasinStartingWith (point : Point) (grid: int[,]) : Set<Point> =
    // BFS up in this, yo
    let pointsToVisit = new Queue<Point>([|point|]) in
    let mutable pointsSeen = Set.empty in
    let mutable currentPoint = point in
    while pointsToVisit.Count > 0 do
        currentPoint <- pointsToVisit.Dequeue()
        if not (pointsSeen.Contains currentPoint) then
            let row, col = currentPoint
            let value = grid[row, col]
            if value <> 9 then
                pointsSeen <- Set.add currentPoint pointsSeen;
                for neighbor in findNeighbors grid currentPoint do
                    pointsToVisit.Enqueue(neighbor)
    pointsSeen


let solve grid =
    let lowPointValues, lowPointCoords = List.unzip <| [
        for row in 0..((Array2D.length1 grid) - 1) do
            for column in 0..((Array2D.length2 grid - 1)) do
                let value = grid[row, column] in
                let neighbors = findNeighbors grid (row, column)
                let neighborValues = neighbors |> Seq.map (fun (row, column) -> grid[row, column])
                if isLowPoint value neighborValues then
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
            basins <- (findBasinStartingWith (row, column) grid) :: basins
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
    let grid = array2D [|
        for line in System.IO.File.ReadAllLines filename do
            yield 
                line
                |> Array.ofSeq
                |> Array.map (string >> int)
    |]
    in
    let partA, partB = solve grid
    printfn "Part A: %d" partA;
    printfn "Part B: %d" partB;
    0