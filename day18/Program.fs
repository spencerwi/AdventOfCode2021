module SnailfishMath = begin
    type t = 
    | Leaf of int
    | Node of left:t * right:t

    module Parsing = begin
        open FParsec

        [<RequireQualifiedAccess>]
        type Element = 
        | IntValue of int
        | Pair of Element list

        let rec buildTree (listRepresentation : Element) =
            match listRepresentation with
            | Element.IntValue i -> Leaf i
            | Element.Pair [rawLeft; rawRight] -> 
                Node ((buildTree rawLeft), (buildTree rawRight))

        let parse input =
            let pSnailfishNumber = 
                let pIntValue = pint32 |>> (fun i -> Element.IntValue i) in
                let pElement, pElementRef = createParserForwardedToRef() in
                let pList = 
                    pchar '[' >>. (sepBy1 pElement (pchar ',')) .>> pchar ']'
                    |>> (fun elements -> Element.Pair elements)
                pElementRef := (pIntValue <|> pList); 
                pList
            in
            match (run pSnailfishNumber input) with
            | Success (result, _, _) -> buildTree result
            | Failure (errMsg, _, _) -> failwith $"Error: {errMsg}"
    end

    let reduce (number : t) =
        let rec go depth n =
            match n with 
            | Leaf value when value >= 10 -> 
                let halfFloat = (float value) / 2.0 in
                Node ((floor halfFloat), (ceil halfFloat))
            | Node (left, right) when depth >= 4 ->


    
end

[<EntryPoint>]
let main argv =
    let input = 
        if argv.Length > 0 then argv[0]
        else (System.IO.File.ReadAllLines "input.txt") |> Array.head
    in
    SnailfishMath.Parsing.parse input
    |> printfn "%A"
    0