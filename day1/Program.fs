// Utility function to perform the repeated task of counting
//  how many times within a list of ints a particular element
//  is larger than its predecessor, since both parts of this
//  problem require that.
let count_increases (input : int array) =
    let iterator (total, prev) current =
        match prev with
        | Some a when a < current -> (total + 1, Some current)
        | _ -> (total, Some current)
    in
    Array.fold iterator (0, None) input
    |> fst

// Part 1: how often is the current number larger than its predecessor?
let part_a = count_increases 

// Part 2: how often is the current three-numbr-window-sum larger than 
//  the previous three-number-window-sum?
let part_b (input: int array) =
    let sliding_sum_list =
        input 
        |> Array.mapi (fun idx current ->
            if idx < 2 then None
            else
                Some (current + input.[idx - 1] + input.[idx - 2])
        ) 
        |> Array.filter (Option.isSome)
        |> Array.map (Option.get)
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