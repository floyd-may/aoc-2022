module aoc_2022.Day9

open System.IO
open FParsec
open ParsingUtils

type Direction =
    | Up
    | Down
    | Left
    | Right

type Instruction = {
    Dir: Direction;
    Count: int;
}

type Coord = int * int
type RopeState = Coord list

let origin = [
    (0, 0); (0, 0);
]

type PuzzleState = {
    Pos: RopeState
    Tails: Set<Coord>
}

module PuzzleState =
    let create pos =
        {
            Pos = pos
            Tails = Set.singleton pos.Head
        }
        
    let update pos state =
        {
            Pos = pos
            Tails =
                state.Tails
                |> Set.add pos.Head
        }
    
let moveTail head tail =
    let headX, headY = head
    let tailX, tailY = tail
    let deltaX = headX - tailX
    let deltaY = headY - tailY
    
    (*
    Tail at point B;
    Head will be at one of the points here:
    points w,x,y,z are only relevant to part 2
    
    w 1 2 3 x
    4 5 6 7 8
    9 A B C D
    E F G H I
    y J K L rz
    *)
    
    if (abs deltaX > 1) && (abs deltaY > 1)
    then // cases w,x,y,z
        (tailX + (deltaX / 2), tailY + (deltaY / 2))
    else if abs deltaX > 1
    then // cases 4, 8, 9, D, E, I
        (tailX + (deltaX / 2), headY)
    else if abs deltaY > 1
    then // cases 1, 2, 3, J, K, L
        (headX, tailY + (deltaY / 2))
    else // everything else in the inner 9 positions
        tail

let rec runInstruction state input =
    if input.Count = 0 then state else
        
    let orderedRope = state.Pos |> List.rev
        
    let headX, headY = orderedRope.Head
    
    let xOffset, yOffset =
        match input.Dir with
        | Up -> (0, 1)
        | Down -> (0, -1)
        | Direction.Left -> (-1, 0)
        | Direction.Right -> (1, 0)
        
    let mutable newRope = [(headX + xOffset, headY + yOffset)]
    
    for tail in orderedRope.Tail do
        let head = newRope.Head
        let newTail = moveTail head tail
        newRope <- newTail :: newRope

    let newState =
        PuzzleState.update newRope state
    
    let newInstruction = {
        input with Count = input.Count - 1
    }
    
    runInstruction newState newInstruction 

module parsing =
    let charToDir c =
        match c with
        | 'U' -> Up
        | 'D' -> Down
        | 'L' -> Direction.Left
        | 'R' -> Direction.Right
        | _ -> failwithf $"unexpected direction %c{c}"
    let parseInstruction =
        tuple2
            (asciiUpper |>> charToDir .>> spaces)
            (pint32 .>> restOfLine true)
        |>> (fun (d,c) -> { Dir = d; Count = c })
        
        
let part1Core lines =
    let instructions =
        lines
        |> List.map (runParser parsing.parseInstruction)
        
    let startState = PuzzleState.create origin
    
    let finalState =
        instructions
        |> List.fold runInstruction startState
    
    //$"%A{finalState.Tails}"
    finalState.Tails |> Set.count |> string
    
let part2Core lines =
    let instructions =
        lines
        |> List.map (runParser parsing.parseInstruction)
        
    let ropeStart = List.replicate 10 (0, 0)
        
    let startState = PuzzleState.create ropeStart
    
    let finalState =
        instructions
        |> List.fold runInstruction startState
    
    //$"%A{finalState.Pos}"
    finalState.Tails |> Set.count |> string
    
let private parseFile () =
    File.ReadAllLines "./day9.txt"
    |> List.ofSeq

let part1 () = part1Core (parseFile())
let part2 () = part2Core (parseFile())
