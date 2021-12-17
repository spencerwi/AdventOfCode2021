open System.Text.RegularExpressions

let (|Regex|_|) regex str =
   let m = (new Regex(regex)).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

module TargetArea = begin
    type Point = {
        x: int;
        y: int;
    }
    type Velocity = {
        dx: int
        dy: int
    }
    type t = {
        left: int
        right: int
        bottom: int
        top: int
    }

    let parse = function
        | Regex "target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)" [leftStr;rightStr;bottomStr;topStr] ->
            {left = (int leftStr); right = (int rightStr); bottom = (int bottomStr); top = (int topStr)}
        | other -> failwith $"Invalid line: {other}"

    let contains area point =
        point.x >= area.left && point.x <= area.right &&
        point.y >= area.bottom && point.y <= area.top

    let hasPassed point targetArea =
        let hasPassedHorizontally =
            if targetArea.right <= 0 then
                point.x < targetArea.left
            else
                point.x > targetArea.right
        in
        let hasPassedVertically =
            if targetArea.top <= 0 then
                point.y < targetArea.bottom
            else
                point.y > targetArea.top
        in
        hasPassedHorizontally && hasPassedVertically

    let willNeverLandIn (point, velocity) targetArea =
        // if our X velocity has fizzled out before we've gotten to the target, we won't land.
        let hasXVelocityProblem =
            (point.x < targetArea.left || point.x > targetArea.right)
            && velocity.dx = 0
        in
        // if our Y velocity is always going to keep us below the target, we won't land.
        let hasYVelocityProblem =
            (point.y < targetArea.bottom) && velocity.dy <= 0
        in
        hasXVelocityProblem || hasYVelocityProblem


    
    let traceLine velocity targetArea =
        let mutable currentPoint = {x=0; y=0} in
        let mutable v = velocity;
        let shouldStop point velocity =
            (targetArea |> contains <| point)
            ||
            (point |> hasPassed <| targetArea)
            || 
            ((point, velocity) |> willNeverLandIn <| targetArea)
        in
        seq {
            while not (shouldStop currentPoint v) do
                currentPoint <- {
                    x = currentPoint.x + v.dx;
                    y = currentPoint.y + v.dy
                }
                let newDx = 
                    if v.dx > 0 then v.dx - 1
                    elif v.dx < 0 then v.dx + 1
                    else v.dx
                v <- {dy = v.dy - 1; dx = newDx}
                yield currentPoint
        }

    let hitsTarget velocity targetArea =
        traceLine velocity targetArea
        |> Seq.exists (targetArea |> contains)
end

let solve input = 
    let target = TargetArea.parse input
    let (xStep, farXEdge) =
        if target.left < 0 then 
            (-1, target.left)
        else 
            (1, target.right)
    in
    let yStep, farYEdge =
        if target.top < 0 then 
            (-1, target.bottom)
        else 
            (1, target.top)
    in
    let onTargetVelocities =
        query {
            for dx in 0..xStep..farXEdge do
            for dy in (farYEdge * -1)..yStep..farYEdge do
            let candidateVelocity : TargetArea.Velocity = {dx=dx; dy=dy}
            where (TargetArea.hitsTarget candidateVelocity target)
            select candidateVelocity
        }
    in
    let partA = 
        onTargetVelocities
        |> Seq.map (fun velocity ->
            let points = TargetArea.traceLine velocity target in
            points |> Seq.map (fun point -> point.y) |> Seq.max
        )
        |> Seq.max
    in
    let partB =
        Set.ofSeq onTargetVelocities |> Set.count
    (partA, partB)

[<EntryPoint>]
let main argv = 
    let input =
        if argv.Length > 0 then argv[0]
        else (System.IO.File.ReadAllLines "input.txt") |> Array.head
    in
    let partA, partB = solve input in
    printfn "Part A: %d" partA
    printfn "Part B: %d" partB
    0