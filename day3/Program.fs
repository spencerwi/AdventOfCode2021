open System

type MostOrLeast =
    | Most
    | Least

let flipBit = function
    | '0' -> '1'
    | '1' -> '0'

let bitstringToInt bs = Convert.ToInt32(bs, 2)

let mostCommonBitInColumn (column: int) (inputs: string[]) =
    let totalNumberOfInputs = Array.length inputs in
    let onesInThisColumn = 
        inputs
        |> Array.filter (fun line -> line[column] = '1')
        |> Array.length
    let zeroesInThisColumn = totalNumberOfInputs - onesInThisColumn in
    if onesInThisColumn >= zeroesInThisColumn then
        '1'
    else
        '0'

let mostAndLeastCommonBits (inputs : string[]) =
    let bitsPerLine = String.length inputs[0] in
    let mostCommonBits = [|
        for column in 0..(bitsPerLine - 1) do
            yield mostCommonBitInColumn column inputs
    |]
    let leastCommonBits = Array.map flipBit mostCommonBits in
    (new System.String(mostCommonBits), new System.String(leastCommonBits))

let findGammaAndEpsilon (inputs : string[]) =
    let mostCommonBits, leastCommonBits = mostAndLeastCommonBits inputs in
    let gamma = bitstringToInt mostCommonBits in 
    let epsilon = bitstringToInt leastCommonBits in
    (gamma, epsilon)

let powerConsumption input = 
    let gamma, epsilon = findGammaAndEpsilon input in
    gamma * epsilon 

let part_a = powerConsumption

let filterByBitCriteria (mostOrLeastCommon: MostOrLeast) (input: string[]) =
    let rec findIt (inputRows: string[]) (columnBeingChecked: int) : string =
        let mostCommonBit = mostCommonBitInColumn columnBeingChecked inputRows in
        let bitToMatch = 
            match mostOrLeastCommon with
            | Most -> mostCommonBit
            | Least -> flipBit mostCommonBit
        let matchingRows = 
            inputRows
            |> Array.filter (fun row -> row[columnBeingChecked] = bitToMatch)
        in
        if Seq.length matchingRows = 1 then 
            Array.head matchingRows
        else
            let newCol = columnBeingChecked + 1
            findIt matchingRows newCol
    in
    let allRows: string[] = input in
    let result = findIt allRows 0 in
    bitstringToInt result

let lifeSupportRating input = 
    let oxygenGeneratorRating = filterByBitCriteria Most input in
    let co2ScrubberRating = filterByBitCriteria Least input in
    oxygenGeneratorRating  * co2ScrubberRating 

let part_b = lifeSupportRating

[<EntryPoint>]
let main argv =
    let input = 
        System.IO.File.ReadAllLines "input.txt"
        |> Array.filter (not << String.IsNullOrWhiteSpace)
    in
    printfn "Part A: %d" (part_a input);
    printfn "Part B: %d" (part_b input)
    0