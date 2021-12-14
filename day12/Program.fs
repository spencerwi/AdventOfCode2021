let isLowerCase (s: string) =
    s.ToLower() = s

module Cave = begin
    type t = Map<string, string seq>

    type TraversalStrategy = {
        canVisit : Set<string> -> string -> bool
        updateCaves : Set<string> -> string -> Set<string>
    }

    let parse (inputLines: string[]) : t =
        seq {
            for line in inputLines do
                match line.Split "-" with
                | [|start; next|] -> 
                    yield (start, next)
                    yield (next, start)
                | _ -> failwith $"Invalid line: {line}"
        } 
        |> Seq.groupBy fst
        |> Map.ofSeq
        |> Map.map (fun _ values -> Seq.map snd values)

    let partAStrategy = 
        let canVisit alreadySeenSmallCaves neighbor =
            if isLowerCase neighbor then
                not (Set.contains neighbor alreadySeenSmallCaves)
            else true
        in
        let updateCaves alreadySeenSmallCaves neighbor =
            if isLowerCase neighbor then
                Set.add neighbor alreadySeenSmallCaves
            else
                alreadySeenSmallCaves
        { canVisit = canVisit; updateCaves = updateCaves }

    let partBStrategy =
        let canVisit alreadySeenSmallCaves neighbor =
            if neighbor = "start" then false
            elif isLowerCase neighbor then
                // First see if we've already visited a cave twice:
                let alreadyRevisitedASmallCave = 
                    alreadySeenSmallCaves 
                    |> Set.exists (fun (cave : string) -> cave.EndsWith "2")
                in
                if alreadyRevisitedASmallCave then
                    not (Set.contains neighbor alreadySeenSmallCaves)
                else
                    not (Set.contains (neighbor + "2") alreadySeenSmallCaves)
            else true
        in
        let updateCaves alreadySeenSmallCaves neighbor =
            if isLowerCase neighbor then
                let alias = 
                    if Set.contains neighbor alreadySeenSmallCaves then
                        neighbor + "2"
                    else
                        neighbor
                in
                Set.add alias alreadySeenSmallCaves
            else
                alreadySeenSmallCaves
        in
        {canVisit = canVisit; updateCaves = updateCaves }

    let findPaths (strategy : TraversalStrategy) (cave : t) =
        // Let's do some DFS backtracking, yay!
        let rec countPaths (pathSoFar: string) (alreadySeenSmallCaves : Set<string>) goal start =
            if start = goal then 1
            else
                match Map.tryFind start cave with
                | None -> 0
                | Some neighbors -> 
                    query {
                        for neighbor in neighbors do
                        where (strategy.canVisit alreadySeenSmallCaves neighbor)
                        let newAlreadySeenCaves = strategy.updateCaves alreadySeenSmallCaves neighbor in
                        let updatedPath = pathSoFar + "->" + neighbor in
                        sumBy (countPaths updatedPath newAlreadySeenCaves goal neighbor)
                    }
        in
        countPaths "start" (Set.ofList ["start"]) "end" "start"
end


[<EntryPoint>]
let main argv =
    let filename = 
        if argv.Length > 0 then argv[0]
        else "input.txt"
    in 
    let cave = 
        System.IO.File.ReadAllLines filename
        |> Cave.parse
    in
    printfn "Part A: %d" (Cave.findPaths Cave.partAStrategy cave)
    printfn "Part B: %d" (Cave.findPaths Cave.partBStrategy cave)
    0