﻿#nowarn "25"
open System

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

    // We next need to determine which digit is 6. This is because we need to 
    //  have identified 6 in order to identify 5 (and 2).
    let digitSix = 
        nonUniqueLengthDigits
        |> Seq.filter (fun digit -> digit.Length = 6)
        |> Seq.find (fun digit -> 
            let digitOne = getDigitFor 1 in
            not (digit |> hasAllWiresFrom <| digitOne)
        )
    markAs digitSix 6;

    // Now we have enough information to identify the remaining digits (2, 3, 5, and 9).
    for digit in nonUniqueLengthDigits do
        match digit.Length with
        | 5 ->
            // This is either a 2, a 3, or a 5.
            // When 3 is lit up, all the lights from 1 are also lit up. 
            // The same is not true when 2 or 5 is lit up. So we can use that to tell them apart.
            let digitOne = getDigitFor 1 in
            if digit |> hasAllWiresFrom <| digitOne then
                markAs digit 3
            else
                // Now this is either a 2 or a 5. 
                // When 6 is lit up, then all the lights from 5 are also lit up.
                if digitSix |> hasAllWiresFrom <| digit then
                    markAs digit 5 
                else
                    markAs digit 2
        | 6 -> 
            // This is either a 0, a 6, or a 9.
            // When 9 is lit up, all the lights from 4 are also lit up. 
            // That's not true for 6 or 0.
            let digitFour = getDigitFor 4 in
            if digit |> hasAllWiresFrom <| digitFour then
                markAs digit 9
            else
                // Now it's either a 0 or a 6.
                // When 0 is lit up, all the lights from 1 are also lit up. 
                // That's not true for 0.
                let digitOne = getDigitFor 1 in
                if digit |> hasAllWiresFrom <| digitOne then
                    markAs digit 0
                else
                    markAs digit 6
    mapping

let partA input =
    seq {
        for line in input do
            for digit in line do
                if isDistinctWireCountDigit digit then
                    yield digit
    } |> Seq.length

let partB (lines: (string[] * string[]) list) =
    seq {
        for (shownDigits, outputDigits) in lines do 
            let digitMapping = buildMapping shownDigits in
            yield 
                outputDigits
                |> Seq.map (fun outputDigit ->
                    Map.find (sortString outputDigit) digitMapping
                    |> string
                )
                |> String.concat ""
                |> int

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
            yield (shown, output)
    ]
    in
    let shownDigits, outputDigits = List.unzip lines in
    printfn "Part A: %d" (partA outputDigits);
    printfn "Part B: %d" (partB lines);
    0