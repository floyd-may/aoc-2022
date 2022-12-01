module aoc_2022.Day1
open System.IO
open FParsec

let private nl = pchar '\n'

let private pElf =
    manyTill (pint32 .>> nl) nl

let pElves =
    let fileLines = File.ReadAllLines "./day1.txt"
    let fileContents = System.String.Join ("\n", fileLines)
    
    match run (manyTill pElf eof) fileContents with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwithf $"parse fail: %s{errorMsg}"

let part1 =
    let elves = pElves
        
    let topCalorieElf =
        elves
        |> List.map List.sum
        |> List.sortDescending
        |> List.head
    
    topCalorieElf |> string

let part2 =
    let elves = pElves
    
    let topThreeElves = 
        elves
        |> List.map List.sum
        |> List.sortDescending
        |> List.take 3
        |> List.sum
    
    topThreeElves |> string
