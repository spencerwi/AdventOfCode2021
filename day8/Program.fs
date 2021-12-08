open System

let isDistinctWireCountDigit (wires : string) =
    match wires.Length with
    | 2 | 4 | 3 | 7 -> true
    | _ -> false

let buildMapping (lineShownDigits : string seq) =
    let mutable mapping = Map.empty in
    for digit in lineShownDigits do
        let value = 
            match digit.Length with 
            | 2 -> 1
            | 3 -> 7
            | 4 -> 4
            | 7 -> 8
            | n -> 
                // TODO when I get time: how can I determine the rest of the digits?
                0 // placeholder 
        in mapping <- Map.add digit value mapping
    mapping

let partA input =
    seq {
        for line in input do
            for digit in line do
                if isDistinctWireCountDigit digit then
                    yield digit
    } |> Seq.length

let partB lines =
    seq {
        for (shownDigits, outputDigits) in lines do 
            let digitMapping = buildMapping shownDigits in
            yield
                outputDigits
                |> Seq.map (fun digit -> Map.find digit digitMapping)
                |> Seq.sum
    } |> Seq.sum

[<EntryPoint>]
let main argv =
    let inputFilename = 
        if argv.Length > 0 then
            argv[0]
        else
            "input.txt"
    let lines = [
        for line in System.IO.File.ReadAllLines inputFilename do
            let [|shownStr; outputStr|] = line.Split '|' in
            let shown = shownStr.Split(' ', StringSplitOptions.RemoveEmptyEntries) in
            let output = outputStr.Split(' ', StringSplitOptions.RemoveEmptyEntries) in
            yield (shown, output)
    ]
    in
    let shownDigits, outputDigits = List.unzip lines in
    printfn "Part A: %d" (partA outputDigits);
    printfn "Part B: %d" (partB lines);
    0