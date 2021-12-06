module Fishes = begin
    // We use an array to represent each "bucket" of fish -- fish whose timer is at 0 go in the "0" bucket, and so on.
    type t = int64[]

    let parseInput (input: string) =
        let counts = 
            input
            |> (fun line -> line.Split ",")
            |> Seq.map int
            |> Seq.countBy id
            |> Map.ofSeq
        in
        let mutable fishBuckets = Array.create 9 0L in
        for i in 0..8 do
            fishBuckets[i] <- 
                match Map.tryFind i counts with
                | Some count -> (int64 count)
                | None -> 0L
        fishBuckets

    let step (fishes: t) =
        let fishToAdd = fishes[0] in
        let mutable newFish = 
            fishes
            |> Array.skip 1 // shift everyone up one bucket
            |> Array.insertAt 8 fishToAdd // fill up our "8" bucket based on how many 0-timer fish we had
        in
        // And set the 0-timer fish to 6 by dropping them back into the "6" bucket
        newFish[6] <- newFish[6] + fishToAdd;
        newFish
end

let solve input = 
    let mutable fishes = input
    for _ in 0..79 do
        fishes <- Fishes.step fishes
    let part_a = Array.sum fishes
    for _ in 80..255 do
        fishes <- Fishes.step fishes
    let part_b = Array.sum fishes
    (part_a, part_b)

[<EntryPoint>]
let main argv =
    let input = 
        System.IO.File.ReadAllLines "input.txt"
        |> Array.head
        |> Fishes.parseInput
    in 
    let part_a, part_b = solve input
    printfn "Part A: %d" part_a
    printfn "Part B: %d" part_b
    0