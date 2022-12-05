module aoc_2022.Day3

open System
open System.IO
open FParsec

let private parseFile = lazy(
    let fileLines = File.ReadAllLines "./day3.txt"
    
    fileLines
    |> Seq.filter (fun x -> String.IsNullOrWhiteSpace(x) |> not)
    |> List.ofSeq
)
    
let private ucBase = Convert.ToInt32 'A'
let private lcBase = Convert.ToInt32 'a'
let getPriority (c: char) =
    let charCode = Convert.ToInt32 c
    if Char.IsUpper(c)
    then
        charCode - ucBase + 27
    else
        charCode - lcBase + 1

let p1SackPriority (sack: string) =
    let mid = (sack |> Seq.length) / 2
    
    let left = sack.Substring(0, mid)
    let right = sack.Substring(mid)
    
    let leftSet = left |> Set.ofSeq
    let rightSet = right |> Set.ofSeq
    
    Set.intersect leftSet rightSet
    |> List.ofSeq
    |> List.map getPriority
    |> List.head
    
    
let part1 () =
    let sacks = parseFile.Force()
    
    let prioritySum =
        sacks
        |> List.map p1SackPriority
        |> List.sum
    
    prioritySum |> string
    
let part2 () =
    let sacks =
        parseFile.Force()
        |> List.map Set
    
    let groups =
        sacks
        |> Seq.chunkBySize 3
        
    let priorities =
        groups
        |> Seq.map Set.intersectMany
        |> Seq.map List.ofSeq
        |> Seq.map List.head
        |> Seq.map getPriority
    
    let prioritySum =
        priorities
        |> Seq.sum
    
    prioritySum |> string
