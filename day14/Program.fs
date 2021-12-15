open System.Collections.Generic

let parseMappings (inputLines : string[]) =
    let mappings = new Dictionary<string, string>() in
    inputLines
    |> Seq.iter (fun line ->
        let [|pair; insert|] = line.Split(" -> ") in
        mappings[pair] <- insert
    )
    mappings


let step (pairCounts: Dictionary<string, int64>) (mappings: Dictionary<string, string>) =
    // My logic in here is wrong. I'm not adding/subtracting the correct amount
    let result = new Dictionary<string, int64>()
    for pairDestroyed in pairCounts.Keys do
        let insert = mappings[pairDestroyed] in
        let howManyOfTheDestroyedPairWereThere = pairCounts[pairDestroyed] in 
        let firstPairCreated = $"{pairDestroyed[0]}{insert}" in
        let secondPairCreated = $"{insert}{pairDestroyed[1]}" in
        result[firstPairCreated] <- result.GetValueOrDefault(firstPairCreated, 0L) + howManyOfTheDestroyedPairWereThere
        result[secondPairCreated] <- result.GetValueOrDefault(secondPairCreated, 0L) + howManyOfTheDestroyedPairWereThere
    result

let mostMinusLeast (pairCounts: Dictionary<string, int64>) (lastLetter: char) =
    let characterCounts = new Dictionary<char, int64>() in
    for entry in pairCounts do
        let pair = entry.Key
        let pairCount = entry.Value
        characterCounts[pair[0]] <- (characterCounts.GetValueOrDefault(pair[0], 0)) + pairCount 
        // Don't count the second one in the pair, because it'll be counted in its next group (since NNC is an NN and an NC)
    
    // Add in the last letter in the whole string, which, since we're only inserting in the middle, never changes!
    characterCounts[lastLetter] <- characterCounts.GetValueOrDefault(lastLetter, 0) + 1L

    let counts = seq {
        for entry in characterCounts do
            yield entry.Value
    }
    let most = Seq.max counts in
    let least = Seq.min counts in
    most - least

let countPairs (polymerTemplate : string) =
    printfn "Counting from %s" polymerTemplate;
    let pairCounts = new Dictionary<string, int64>();
    polymerTemplate
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> $"{a}{b}")
    |> Seq.countBy id
    |> Seq.iter (fun (pair, count) -> pairCounts[pair] <- (int64 count));
    printfn "%A" pairCounts
    pairCounts


let solve (pairCounts : Dictionary<string, int64>) (mappings: Dictionary<string, string>) (lastLetter: char) =
    let mutable countsSoFar = pairCounts in
    for _ in 1..10 do
        countsSoFar <- step countsSoFar mappings
    let partA = mostMinusLeast countsSoFar lastLetter
    for _ in 11..40 do
        countsSoFar <- step countsSoFar mappings
    (partA, mostMinusLeast countsSoFar lastLetter)

[<EntryPoint>]
let main argv =
    let filename = 
        if argv.Length > 0 then argv[0] 
        else "input.txt"
    in
    let lines = System.IO.File.ReadAllLines filename in
    let polymerTemplate = lines[0] in
    let pairCounts = countPairs polymerTemplate
    let mappings = parseMappings (Array.skip 2 lines) in
    let partA, partB = solve pairCounts mappings (Seq.last polymerTemplate)
    printfn "Part A: %d" partA
    printfn "Part B: %d" partB
    0
    
    