module aoc_2022.ParsingUtils

open FParsec

type StatelessParser<'a> = Parser<'a, unit>

type SkipParser = StatelessParser<unit>

let nl: SkipParser = skipChar '\n'
let space: SkipParser = skipChar ' '
let space2: SkipParser = space >>. space

let runParser parser input =
    match run parser input with
    | Success (result, _, _) -> result
    | Failure (msg, _, _) -> failwithf $"Parse fail: %s{msg}"
    
let runParserWithState parser input initialState =
    match runParserOnString parser initialState "input" input with
    | Success (result, _, _) -> result
    | Failure (msg, _, _) -> failwithf $"Parse fail: %s{msg}"
