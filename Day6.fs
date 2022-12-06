module aoc_2022.Day6

open System
open System.IO

let private parseFile = lazy (
    File.ReadAllLines "./day6.txt"
    |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace(x)))
    |> Seq.head
)

let private allDifferent items =
    let len = items |> List.length
    
    let uniqLen =
        items
        |> List.distinct
        |> List.length
    
    len = uniqLen
    
    
let private leadingNUnique n items =
    if items |> List.length < n then false
    else
    let uniqLen =
        items
        |> List.take n
        |> List.distinct
        |> List.length
        
    n = uniqLen
    
let findPacketStart len input =
    let rec findStartOfPacket prevCharCount packet =
        match packet with
        | _ when leadingNUnique len packet -> prevCharCount + len
        | _ :: rest -> findStartOfPacket (prevCharCount + 1) rest
        | _ -> failwith "no start of packet found"
        
    findStartOfPacket 0 (input |> List.ofSeq)

let part1Core input = findPacketStart 4 input

let part1 () =
    parseFile.Force()
    |> part1Core
    |> string
    
let part2 () =
    parseFile.Force()
    |> findPacketStart 14
    |> string
