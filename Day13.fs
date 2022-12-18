module aoc_2022.Day13


type PacketList = List<PacketElement>

and PacketElement =
    | PNum of int
    | PList of PacketList
    

module Parsing =
    open FParsec
    open ParsingUtils
    let private pPacket =
        let leftBrace = skipChar '['
        let rightBrace = skipChar ']'
        let inBraces x = between leftBrace rightBrace x
        
        let pPacketElem, pPacketElemRef = createParserForwardedToRef<PacketElement, unit>()
        
        let pList =
            sepBy pPacketElem (skipChar ',')
            |> inBraces
            |>> PList
            
        pPacketElemRef.Value <- pList <|> (pint32 |>> PNum)
        
        pList
    
    let private pAllInput =
        let packetWithNewline = pPacket .>> (skipNewline <|> eof)
        let pPair = tuple2 packetWithNewline packetWithNewline
        
        sepBy pPair skipNewline
        
    let parseInput text =
        runParser pAllInput text

let rec comparePackets pair =
    match pair with
    | PNum x, PNum y -> x.CompareTo(y)
    | PList [], PList _ -> -1
    | PList _, PList [] -> 1
    | PNum x, PList ys -> comparePackets (PList [PNum x], PList ys)
    | PList xs, PNum y -> comparePackets (PList xs, PList [PNum y])
    | PList leftList, PList rightList ->
        let headOrdered = comparePackets (leftList.Head, rightList.Head)
        
        if headOrdered <> 0
        then headOrdered
        else comparePackets (PList leftList.Tail, PList rightList.Tail)
        

let part1core (input: string list) =
    let text = System.String.Join("\n", input)
    
    let packets = Parsing.parseInput text
    
    let cmpToIsOrdered x = x <= 0
    
    packets
    |> Seq.map comparePackets
    |> Seq.indexed
    |> Seq.filter (snd >> cmpToIsOrdered)
    |> Seq.map fst
    |> Seq.map (fun x -> x + 1)
    |> Seq.sum

let part1 () =
    let lines =
        System.IO.File.ReadAllLines("./day13.txt")
        |> List.ofSeq
        
    part1core lines
    |> string
