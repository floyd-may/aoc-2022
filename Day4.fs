module aoc_2022.Day4

open System
open System.IO
open FParsec
open ParsingUtils

type Assignment = {
    Start: int
    End: int;
}

let parseAssignment =
    let pStart = pint32 .>> skipChar '-'
    
    pStart .>>. pint32
    |>> fun x -> { Start = fst x; End = snd x; }
    
let parseLine line =
    let pStart = parseAssignment .>> skipChar ','
    
    let rawParser = pStart .>>. parseAssignment
    
    runParser rawParser line

let contains x y =
    x.Start <= y.Start && x.End >= y.End
    
let overlaps x y =
    let left,right = if x.Start <= y.Start then x,y else y,x
    
    let result = left.End >= right.Start
    
    result
    
let hasFullContainment pair =
    let left = fst pair
    let right = snd pair
    
    contains left right || contains right left
    
let private parseFile = lazy(
    let fileLines = File.ReadAllLines "./day4.txt"
    
    fileLines
    |> Seq.filter (fun x -> String.IsNullOrWhiteSpace(x) |> not)
    |> Seq.map parseLine
    |> List.ofSeq
)

let part1 () =
    let pairs = parseFile.Force()
    
    pairs
    |> List.filter hasFullContainment
    |> List.length
    |> string
    
let part2 () =
    let pairs = parseFile.Force()
    
    pairs
    |> List.filter (fun x -> overlaps (fst x) (snd x))
    |> List.length
    |> string
