open System


module Packets = begin
    type Length = Packets of int | Characters of int
    type LiteralValuePacket = {
        version: int
        value: int64
    } and OperatorPacket = {
        version: int
        length: Length
        subpackets: Packet list
    } and Packet = Literal of LiteralValuePacket | Operator of OperatorPacket

    let isJunkData bits =
        not (Seq.exists ((=) '1') bits)

    let rec parsePacket indent (bits : string) : (string * Packet) option =
        if isJunkData bits then
            None // this is just junk data at the end
        else 
            printf "%s%s" (String.replicate indent " ") bits
            let version = Convert.ToInt32(bits.Substring(0, 3), 2) in
            let typeId = Convert.ToInt32(bits.Substring(3, 3), 2) in
            match typeId with
            | 4 -> 
                let allButLastGroup = 
                    bits[6..] 
                    |> Seq.chunkBySize 5
                    |> Seq.takeWhile (fun chunk -> chunk[0] <> '0') 
                    |> Seq.map (fun chars -> new String(chars[1..]))
                    |> String.concat ""
                in
                let lastGroup = 
                    bits[6..]
                    |> Seq.chunkBySize 5
                    |> Seq.skipWhile(fun chunk -> chunk[0] <> '0')
                    |> Seq.head
                    |> (fun chars -> new String(chars[1..]))
                let literalValueStr = allButLastGroup + lastGroup in
                let literalValue = Convert.ToInt64(literalValueStr, 2) in
                printfn " Literal %d" literalValue
                let totalPacketLength = 6 + literalValueStr.Length
                Some (bits[totalPacketLength + 1..], Literal {version = version; value = literalValue})
            | _ -> 
                let length = 
                    match bits[6] with
                    | '0' -> Characters (Convert.ToInt32(bits.Substring(7, 15), 2))
                    | '1' -> Packets (Convert.ToInt32(bits.Substring(7, 11), 2))
                in
                printfn " Operator length=%A" length 
                let leftoverBits, subpackets =
                    match length with
                    | Characters count ->
                        let mutable remainingBitsInPacket = bits.Substring(22, count) in
                        (
                            bits.Substring(22 + count),
                            [
                                let mutable newIndent = indent + 22 
                                while String.length remainingBitsInPacket > 0 do
                                    match parsePacket newIndent remainingBitsInPacket with
                                    | Some (unparsedBitsInPacket, subpacket) ->
                                        newIndent <- newIndent + (remainingBitsInPacket.Length - unparsedBitsInPacket.Length)
                                        remainingBitsInPacket <- unparsedBitsInPacket
                                        yield subpacket
                                    | None -> remainingBitsInPacket <- ""
                            ]
                        )
                    | Packets count ->
                        let mutable remainingBitsInPacket = bits[18..] in
                        //printfn "Remaining bits in the packet: %s" remainingBitsInPacket
                        let subpacketList = [
                            let mutable newIndent = indent + 18
                            for _ in 1..count do
                                match parsePacket newIndent remainingBitsInPacket with
                                | Some (unparsedBitsFromPacket, subpacket) ->
                                    newIndent <- newIndent + (remainingBitsInPacket.Length - unparsedBitsFromPacket.Length)
                                    remainingBitsInPacket <- unparsedBitsFromPacket
                                    yield subpacket
                                | None -> ()
                        ] in
                        //printfn "Remaining bits in the packet: %s" remainingBitsInPacket
                        (remainingBitsInPacket, subpacketList)
                in
                Some (leftoverBits, Operator {version = version; length = length; subpackets = subpackets})


    let hexadecimalToBits input =
        input 
        |> Seq.map (function 
            | '0' -> "0000"
            | '1' -> "0001"
            | '2' -> "0010"
            | '3' -> "0011"
            | '4' -> "0100"
            | '5' -> "0101"
            | '6' -> "0110"
            | '7' -> "0111"
            | '8' -> "1000"
            | '9' -> "1001"
            | 'A' -> "1010"
            | 'B' -> "1011"
            | 'C' -> "1100"
            | 'D' -> "1101"
            | 'E' -> "1110"
            | 'F' -> "1111"
        )
        |> String.concat ""

    let rec versionSum = function
        | Literal l -> l.version
        | Operator o -> 
            o.version + (Seq.sumBy versionSum o.subpackets)

end

[<EntryPoint>]
let main argv =
    let input = 
        if argv.Length > 0 then argv[0]
        else
            System.IO.File.ReadAllLines "input.txt"
            |> Array.head
    in
    let (_, rootPacket) = 
        input
        |> Packets.hexadecimalToBits
        |> Packets.parsePacket 0 
        |> Option.get
    printfn "%A" rootPacket
    printfn "Part A: %d" (Packets.versionSum rootPacket)
    // printfn "Part A: %d" (partA input)
    0