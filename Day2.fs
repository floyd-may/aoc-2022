module aoc_2022.Day2
open System.IO
open FParsec

let private nl = skipChar '\n'

type private OpponentChoice =
    | A
    | B
    | C

type private Response =
    | X
    | Y
    | Z
    
type RPS = Rock | Paper | Scissors

let private rpsFromOpp opp =
    match opp with
    | A -> Rock
    | B -> Paper
    | C -> Scissors
    
let private rpsFromResp resp =
    match resp with
    | X -> Rock
    | Y -> Paper
    | Z -> Scissors

let private pOpponent =
    choice [
        skipChar 'A' >>. preturn A
        skipChar 'B' >>. preturn B
        skipChar 'C' >>. preturn C
    ]
    
let private pResponse =
    choice [
        skipChar 'X' >>. preturn X
        skipChar 'Y' >>. preturn Y
        skipChar 'Z' >>. preturn Z
    ]
    
let private scoreRps rps =
    match rps with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
    
let private scoreRound r =
    let outcomeScore = 
        match r with
        | (x, y) when x = y -> 3
        | (Rock, Paper)
        | (Paper, Scissors)
        | (Scissors, Rock) -> 6
        | _ -> 0
    
    outcomeScore + scoreRps (snd r)
    
let private pRound = 
    (pOpponent .>> spaces) .>>. pResponse .>> (nl <|> eof)
    
let private parseFile =
    let fileLines = File.ReadAllLines "./day2.txt"
    let fileContents = System.String.Join ("\n", fileLines)
    
    match run (manyTill pRound eof) fileContents with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwithf $"parse fail: %s{errorMsg}"
    
let part1 =
    let rounds = parseFile
    
    let decodedRounds =
        rounds
        |> List.map (fun x -> (rpsFromOpp (fst x), rpsFromResp (snd x)))

    decodedRounds
    |> List.map scoreRound
    |> List.sum
    |> string

let private decodeRound r =
    match r with
    | (x, Y) -> (x, x)
    | (Rock, X) -> (Rock, Scissors)
    | (Paper, X) -> (Paper, Rock)
    | (Scissors, X) -> (Scissors, Paper)
    | (Rock, Z) -> (Rock, Paper)
    | (Paper, Z) -> (Paper, Scissors)
    | (Scissors, Z) -> (Scissors, Rock)
let part2 =
    let rounds = parseFile
    
    let decodedRounds =
        rounds
        |> List.map (fun x -> (rpsFromOpp (fst x), snd x))
        |> List.map decodeRound

    decodedRounds
    |> List.map scoreRound
    |> List.sum
    |> string
