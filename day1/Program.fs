// Utility function to perform the repeated task of counting
//  how many times within a list of ints a particular element
//  is larger than its predecessor, since both parts of this
//  problem require that.
let count_increases (input : int array) =
    input
    |> Array.pairwise
    |> Array.filter (fun (prev, current) -> current > prev)
    |> Array.length

// Part 1: how often is the current number larger than its predecessor?
let part_a = count_increases 

// Part 2: how often is the current three-number-window-sum larger than 
//  the previous three-number-window-sum?
let part_b (input: int array) =
    let sliding_sum_list =
        input 
        |> Array.windowed 3
        |> Array.map Array.sum
    in
    count_increases sliding_sum_list
    

[<EntryPoint>]
let main argv =
    let input = 
        System.IO.File.ReadAllLines "input.txt" 
        |> Array.map int
    in
    printfn "Part A: %d" (part_a input);
    printfn "Part B: %d" (part_b input);
    0