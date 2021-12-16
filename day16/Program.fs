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
                let literalValueSection =
                    let allButLastGroup = 
                        bits[6..]
                        |> Seq.chunkBySize 5
                        |> Seq.takeWhile (fun group -> group[0] <> '0')
                    let lastGroup = 
                        bits[6..]
                        |> Seq.chunkBySize 5
                        |> Seq.skipWhile (fun group -> group[0] <> '0')
                        |> Seq.head
                    in seq {
                        yield! allButLastGroup
                        yield lastGroup
                    }
                let literalValueSectionLength =
                    (Seq.length literalValueSection) * 5
                let valueStr = 
                    literalValueSection
                    |> Seq.map (fun group -> group[1..])
                    |> Seq.map (fun group -> new String(group))
                    |> String.concat ""
                let value = Convert.ToInt64(valueStr, 2)
                printfn " Literal %d" value
                let totalPacketLength = 6 + literalValueSectionLength
                Some (bits[totalPacketLength..], Literal {version = version; value = value})
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
                        let remainingBitsAfterThisPacket = bits.Substring(22 + count) in
                        let mutable remainingBitsInThisPacket = bits.Substring(22, count) in
                        (
                            remainingBitsAfterThisPacket,
                            [
                                let mutable newIndent = indent + 22 
                                while String.length remainingBitsInThisPacket > 0 do
                                    match parsePacket newIndent remainingBitsInThisPacket with
                                    | Some (unparsedBitsInPacket, subpacket) ->
                                        newIndent <- newIndent + (remainingBitsInThisPacket.Length - unparsedBitsInPacket.Length)
                                        remainingBitsInThisPacket <- unparsedBitsInPacket
                                        yield subpacket
                                    | None -> remainingBitsInThisPacket <- "" // stop looping
                            ]
                        )
                    | Packets count ->
                        let mutable allRemainingBits = bits[18..] in
                        //printfn "Remaining bits in the packet: %s" remainingBitsInPacket
                        let subpacketList = [
                            let mutable newIndent = indent + 18
                            for _ in 1..count do
                                match parsePacket newIndent allRemainingBits with
                                | Some (unparsedBitsFromPacket, subpacket) ->
                                    newIndent <- newIndent + (allRemainingBits.Length - unparsedBitsFromPacket.Length)
                                    allRemainingBits <- unparsedBitsFromPacket
                                    yield subpacket
                                | None -> ()
                        ] in
                        //printfn "Remaining bits in the packet: %s" remainingBitsInPacket
                        (allRemainingBits, subpacketList)
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
    printfn "Part A: %d" (Packets.versionSum rootPacket)
    // printfn "Part A: %d" (partA input)
    0