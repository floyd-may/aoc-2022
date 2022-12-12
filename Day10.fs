module aoc_2022.Day10

open System.IO
open FParsec
open ParsingUtils

type Instr =
    | Noop
    | AddX of int

let runInstr x instr =
    match instr with
    | Noop -> [x]
    | AddX arg -> [x;x + arg]
    
let runProgram instructions =
    seq {
        let mutable register = 1
        // "cheating" here so that indexing starts at 1;
        // we can just skip the first item
        yield register // index 0
        yield register // index 1
        for instr in instructions do
            for res in (runInstr register instr) do
                register <- res
                yield res
    }
    |> Seq.indexed
    |> Seq.skip 1

let getSignalStrengths instructions =
    seq {
        let mutable targetState = 20
        for idx, register in runProgram instructions do
            if idx = targetState then
                targetState <- targetState + 40
                yield idx * register
    }
    
let parseNoop =
    skipString "noop"
    .>> restOfLine true
    >>. preturn Noop
    
let parseAddx =
    skipString "addx "
    >>. pint32
    .>> restOfLine true
    |>> AddX
    
let parseInstr line =
    runParser (parseNoop <|> parseAddx) line
    
let part1core lines =
    let instructions =
        lines
        |> List.map parseInstr
        
    getSignalStrengths instructions
    |> Seq.sum

let part1 () =
    let lines =
        File.ReadAllLines "./day10.txt"
        |> List.ofSeq

    part1core lines |> string


type RenderingState = {
    Cycle: int
    XRegister: int;
}

module RenderingState =
    let private pixelPosition cycle = (cycle - 1) % 40
    let ofPair p =
        let cycle, x = p
        {
            Cycle = cycle
            XRegister = x
        }

    let renderPixel st =
        let pixelPos = pixelPosition st.Cycle
        let spritePos =
            [
                st.XRegister - 1;
                st.XRegister;
                st.XRegister + 1;
            ]
            |> Set.ofList
        
        if Set.contains pixelPos spritePos then
            "#"
        else
            " "
    
let part2core lines =
    let instructions =
        lines
        |> List.map parseInstr
        
    runProgram instructions
    |> Seq.map RenderingState.ofPair
    |> Seq.map RenderingState.renderPixel
    |> Seq.chunkBySize 40
    |> Seq.map (fun x -> System.String.Join("", x))
    |> Seq.filter (fun x -> x.Length = 40)
    |> List.ofSeq
    
let part2 () =
    let lines =
        File.ReadAllLines "./day10.txt"
        |> List.ofSeq

    let outputLines = "" :: part2core lines
    
    System.String.Join("\n", outputLines)
