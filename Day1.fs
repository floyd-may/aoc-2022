module aoc_2022.Day1
open System.IO
open FParsec
open ParsingUtils

let private pElf =
    manyTill (pint32 .>> nl) (attempt eof <|> nl)

let pElves =
    let fileLines = File.ReadAllLines "./day1.txt"
    let fileContents = System.String.Join ("\n", fileLines)
    
    runParser (manyTill pElf eof) fileContents

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
