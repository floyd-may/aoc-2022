module aoc_2022.Day5

open System
open System.IO
open FParsec
open ParsingUtils

type Instruction = {
    From: int;
    To: int;
    Count: int;
}

module parsing =
    let private parseBlank = pstring "   " >>. preturn None

    let private parseCrate =
        between (pchar '[') (pchar ']') (regex "[A-Z]")
        |>> Some

    let private parseCrateLine =
        sepBy (attempt parseCrate <|> parseBlank) (skipChar ' ')
        .>> nl
        
    let private parseCrateLabels =
        let parseLbl = space >>. pint32 .>> space
        
        sepBy parseLbl space
        .>> nl

    let pivotCrates crates =
        let lines = fst crates
        
        let fixStack lst =
            lst
            |> List.map snd
            |> List.collect Option.toList
        
        lines
        |> List.map List.indexed
        |> List.collect id
        |> List.groupBy fst
        |> List.map (fun x -> (1 + fst x, snd x |> fixStack))
        |> Map.ofList

    let private parseCrateConfig =
        many parseCrateLine
        .>>. parseCrateLabels
        |>> pivotCrates
        
    let private parseInstruction =
        tuple3
            (skipString "move " >>. pint32)
            (skipString " from " >>. pint32)
            (skipString " to " >>. pint32 .>> (nl <|> eof))
        |>> fun (c, f, t) -> { Count = c; From = f; To = t }
        
    let parseAll =
        tuple2
            (parseCrateConfig .>> nl)
            (many parseInstruction)
        
let private parseFile = lazy(
    let fileLines = File.ReadAllLines "./day5.txt"
    
    let fileContents = String.Join("\n", fileLines)
    
    runParser parsing.parseAll fileContents
)

let move srcKey dstKey (crates: Map<int, string list>) =
    let srcStack = crates[srcKey]
    let dstStack = crates[dstKey]
    
    let newSrcStack = srcStack |> List.tail
    let newDstStack = (List.head srcStack) :: dstStack
    
    crates
    |> Map.add srcKey newSrcStack
    |> Map.add dstKey newDstStack

let rec execInstruction instruction crates =
    if instruction.Count = 0
    then crates
    else
    
    let newCrates = move instruction.From instruction.To crates
    
    execInstruction { instruction with Count = instruction.Count - 1 } newCrates
    
let rec execInstructions1 instructions crates =
    match instructions with
    | [] -> crates
    | i :: rest -> execInstructions1 rest (execInstruction i crates)
    
let moveMany count srcKey dstKey (crates: Map<int, string list>) =
    let srcStack = crates[srcKey]
    let dstStack = crates[dstKey]
    
    let toMove =
        srcStack
        |> List.take count
    
    let newSrcStack = srcStack |> List.skip count
    let newDstStack = toMove @ dstStack
    
    crates
    |> Map.add srcKey newSrcStack
    |> Map.add dstKey newDstStack


let rec execInstructions2 instructions crates =
    match instructions with
    | [] -> crates
    | i :: rest ->
    let newCrates = moveMany i.Count i.From i.To crates
    execInstructions2 rest newCrates
    
let part1 () =
    let crates, instructions = parseFile.Force()
    
    execInstructions1 instructions crates
    |> Map.toList
    |> List.sortBy fst
    |> List.map snd
    |> List.map List.head
    |> fun x -> String.Join("", x)
    
let part2 () =
    let crates, instructions = parseFile.Force()
    
    execInstructions2 instructions crates
    |> Map.toList
    |> List.sortBy fst
    |> List.map snd
    |> List.map List.head
    |> fun x -> String.Join("", x)
