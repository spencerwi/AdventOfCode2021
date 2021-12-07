let fixedCost crabs position =
    crabs
    |> Seq.map (fun crab -> abs (crab - position))
    |> Seq.sum

// The problem describes a situation where each step
//  costs "itself" in gas -- so step 1 costs 1 gas,
//  step 2 costs 2 gas, etc, and you add them together.
// So to move 3 steps, it costs 1 + 2 + 3 = 6 gas.
// As it turns out, this "factorial but with addition"
//  has a name: the binomial coefficient (thanks, Pascal!)
//  and is calculated as (n * (n+1))/2
let binomialCost crabs position =
    let binomial n = 
        (n * (n+1)) / 2
    in
    crabs
    |> Seq.map (fun crab -> 
        let stepsToMove = abs (crab - position) in
        let result = binomial stepsToMove
        result
    )
    |> Seq.sum

let solve (costFunction: int seq -> int -> int) input =
    let highestPosition = Seq.max input in
    seq { 0..highestPosition }
    |> Seq.map (costFunction input)
    |> Seq.min

let part_a = solve fixedCost
let part_b = solve binomialCost


[<EntryPoint>]
let main argv =
    let inputLine = 
        if argv.Length > 0 then
            argv[0]
        else 
            System.IO.File.ReadAllLines("input.txt")
            |> Seq.head
    let input = 
        inputLine.Split(',')
        |> Seq.map int
    in
    printfn "Part A: %d" (part_a input)
    printfn "Part B: %d" (part_b input)
    0