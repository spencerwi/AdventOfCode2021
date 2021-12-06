open System

// Utility functions for dealing with 2D arrays
let rows (arr : 'a[,]) = 
    let height = Array2D.length1 arr in
    seq {
        for row in 0..(height - 1) do
            yield arr[row, *]
    }

let cols (arr: 'a[,]) =
    let width = Array2D.length2 arr in
    seq {
        for col in 0..(width - 1) do
            yield arr[*, col]
    }

let allElements (arr: 'a[,]) =
    seq {
        for row in rows arr do
            yield! row
    }

module BingoBoard = begin
    type cellState = Marked | Unmarked
    type cell = (int * cellState)
    type t = cell[,]

    let parse (inputLines : string array): t = 
        inputLines
        |> Array.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries))
        |> Array.map (Array.map (fun strContents -> (int strContents, Unmarked)))
        |> array2D

    let mark (number: int) (board : t) =
        for row in 0..((Array2D.length1 board) - 1) do
            for col in 0..((Array2D.length2 board) - 1) do
                let (num, _) = board[row, col] in
                if num = number then   
                    board[row, col] <- (number, Marked)

    let isWinner (board : t) : bool =
        let checkGroup = Seq.forall (fun (_, state) -> state = Marked) in
        (
            Seq.exists checkGroup (rows board) 
            ||
            Seq.exists checkGroup (cols board)
        )

    let score (lastNumberCalled : int) (board : t) =
        let unmarkedCellsSum = 
            allElements board 
            |> Seq.filter (fun (_, state) -> state = Unmarked)
            |> Seq.sumBy (fun (num, _) -> num)
        in
        unmarkedCellsSum * lastNumberCalled
end

let rec solve (scores : int[]) (boards : BingoBoard.t[]) (numbers : int[]) =
    if Array.isEmpty boards then
        scores
    else
        let numberCalled = Array.head numbers in
        Array.iter (BingoBoard.mark numberCalled) boards;
        let winners, losers = Array.partition BingoBoard.isWinner boards in
        let winnerScores = Array.map (BingoBoard.score numberCalled) winners in
        let newScores = Array.append scores winnerScores in
        solve newScores losers (Array.tail numbers)

[<EntryPoint>]
let main argv =
    let inputLines = System.IO.File.ReadAllLines "input.txt" 

    let numbersCalled = 
        inputLines
        |> Array.head 
        |> (fun s -> s.Split ',')
        |> Array.map int
    
    let bingoBoards =
        inputLines
        |> Array.tail
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.chunkBySize 5
        |> Array.map BingoBoard.parse

    let resultScores = solve [||] bingoBoards numbersCalled in
    printfn "Part A: %d" (Array.head resultScores)
    printfn "Part B: %d" (Array.last resultScores)
    0