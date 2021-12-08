#nowarn "25"
open System

type RawLine = {
    shown: string[]
    output: string[]
}

let sortString (s: string) : string =
    s |> Seq.sort |> String.Concat

let isDistinctWireCountDigit (wires : string) =
    match wires.Length with
    | 2 | 4 | 3 | 7 -> true
    | _ -> false

let buildMapping (lineShownDigits : string seq) =
    let mutable mapping = Map.empty in
    // Start with the digits we know for certain, because we need those to make 
    // decisions about other digits.
    let markAs (digit : string) value = 
        // The wires in a given digit can appear in any order. 
        // So we apply sorting for consistency.
        mapping <- Map.add (sortString digit) value mapping
    let mutable nonUniqueLengthDigits = [];
    for digit in lineShownDigits do
        match digit.Length with 
        | 2 -> markAs digit 1
        | 3 -> markAs digit 7
        | 4 -> markAs digit 4
        | 7 -> markAs digit 8
        | _ -> 
            nonUniqueLengthDigits <- (digit :: nonUniqueLengthDigits)
    
    // Now that we've nailed down the "certain" digits, we can use those to make
    //  deductions about the remaining digits whose lengths are not unique.
    let getDigitFor value = Map.findKey (fun _ v -> v = value) mapping in
    let hasAllWiresFrom (digit : string) (from : string) : bool =
        from |> String.forall (digit.Contains)
    let digitsOfLength n = 
        nonUniqueLengthDigits
        |> Seq.filter (fun d -> d.Length = n)

    // We next need to determine which digit is 6. This is because we need to 
    //  have identified 6 in order to identify 5 (and 2). Along the way, we'll
    //  wind up identifying 0 and 9.
    for digit in digitsOfLength 6 do
        // This is either a 0, a 6, or a 9.
        // When 9 is lit up, all the lights from 4 are also lit up. 
        // That's not true for 6 or 0.
        if digit |> hasAllWiresFrom <| (getDigitFor 4) then
            markAs digit 9
        else
            // Now it's either a 0 or a 6.
            // When 0 is lit up, all the lights from 1 are also lit up. 
            // That's not true for 0.
            if digit |> hasAllWiresFrom <| (getDigitFor 1) then
                markAs digit 0
            else
                markAs digit 6


    // Now we have enough information to identify the remaining digits (2, 3, and 5).
    for digit in digitsOfLength 5 do
        // This is either a 2, a 3, or a 5.
        // When 3 is lit up, all the lights from 1 are also lit up. 
        // The same is not true when 2 or 5 is lit up. So we can use that to tell them apart.
        if digit |> hasAllWiresFrom <| (getDigitFor 1) then
            markAs digit 3
        else
            // Now this is either a 2 or a 5. 
            // When 6 is lit up, then all the lights from 5 are also lit up.
            if (getDigitFor 6) |> hasAllWiresFrom <| digit then
                markAs digit 5 
            else
                markAs digit 2
    mapping

let translate mapping digits =
    digits
    |> Seq.map (fun digit -> Map.find (sortString digit) mapping)
    |> Seq.map string
    |> String.concat ""
    |> int

let partA input =
    seq {
        for line in input do
            for digit in line do
                if isDistinctWireCountDigit digit then
                    yield digit
    } |> Seq.length

let partB (lines: RawLine list) =
    seq {
        for line in lines do 
            let digitMapping = buildMapping line.shown in
            yield translate digitMapping line.output
    } 
    |> Seq.sum

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
            yield {shown = shown; output = output}
    ]
    in
    let outputDigits = List.map (fun line -> line.output) lines in
    printfn "Part A: %d" (partA outputDigits);
    printfn "Part B: %d" (partB lines);
    0