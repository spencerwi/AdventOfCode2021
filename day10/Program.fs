open System.Collections.Generic

type ParseResult = {
    err: char option
    unmatched: char Stack
}

let scoreError = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let scoreIncomplete stack = 
    let valueOf = function
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' -> 4L
        | _ -> 0L
    in
    stack
    |> Seq.fold (fun total next ->
        (total * 5L) + valueOf next
    ) 0L

let findError line =
    let expectedClosers = new Stack<char>() in
    let unexpectedChar = 
        line
        |> Seq.tryFind (function 
            | '(' -> expectedClosers.Push ')'; false
            | '[' -> expectedClosers.Push ']'; false
            | '{' -> expectedClosers.Push '}'; false
            | '<' -> expectedClosers.Push '>'; false
            | other -> 
                // this must be a closer
                let nextExpectedCloser = expectedClosers.Pop() in
                nextExpectedCloser <> other
        )
    in
    { err = unexpectedChar; unmatched = expectedClosers }


let solve input =
    let results = Array.map findError input in
    let partA = 
        query {
            for result in results do
            where (Option.isSome result.err)
            sumBy (scoreError result.err.Value)
        } 
    in
    let incompleteLineScores = 
        query {
            for result in results do
            where (Option.isNone result.err)
            let score = scoreIncomplete result.unmatched
            sortBy score
            select score
        } |> Array.ofSeq
    in
    let partB = incompleteLineScores[incompleteLineScores.Length / 2] in
    (partA, partB)


[<EntryPoint>] 
let main argv =
    let filename = 
        if argv.Length > 0 then argv[0]
        else "input.txt"
    let input = System.IO.File.ReadAllLines filename in
    let partA, partB = solve input
    printfn "Part A: %d" partA
    printfn "Part B: %d" partB
    0