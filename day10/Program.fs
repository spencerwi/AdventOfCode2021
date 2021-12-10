open System.Collections.Generic

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
    (unexpectedChar, expectedClosers)


let solve input =
    let results = 
        input
        |> Array.map findError
    in
    let partA = 
        results
        |> Seq.map fst
        |> Seq.filter Option.isSome
        |> Seq.sumBy (function 
            | Some c -> scoreError c
            | None -> 0 // shouldn't be reachable, but makes the compiler happy
        )
    let incompleteLineScores = 
        Array.sort <| [| 
            for (error, remaining) in results do
                if Option.isNone error then
                    yield scoreIncomplete remaining
        |]
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