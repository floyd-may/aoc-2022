module aoc_2022.Day9Tests

open Xunit
open FsUnit.Xunit
open Day9
open aoc_2022.Day9

type MovementTestCase = {
    Name: string;
    Start: RopeState;
    Input: Instruction;
    ExpectedPos: RopeState;
    ExpectedVisited: Set<Coord>;
}

let testCases() =
    [
        {
            Name = "Up from overlap";
            Start = origin;
            Input = { Dir = Up; Count = 1 };
            ExpectedPos = [(0, 0); (0, 1)]
            ExpectedVisited = [origin.Head] |> Set.ofList
        };
        {
            Name = "Down from overlap";
            Start = origin;
            Input = { Dir = Down; Count = 1 };
            ExpectedPos = [(0, 0); (0, -1)]
            ExpectedVisited = [origin.Head] |> Set.ofList
        };
        {
            Name = "Left from overlap";
            Start = origin;
            Input = { Dir = Direction.Left; Count = 1 };
            ExpectedPos = [(0, 0); (-1, 0)]
            ExpectedVisited = [origin.Head] |> Set.ofList
        };
        {
            Name = "Right from overlap";
            Start = origin;
            Input = { Dir = Direction.Right; Count = 1 };
            ExpectedPos = [(0, 0); (1, 0)]
            ExpectedVisited = [origin.Head] |> Set.ofList
        };
        {
            Name = "Drag up";
            Start = [(0, -1); (0, 0)]
            Input = { Dir = Up; Count = 1 };
            ExpectedPos = [(0, 0); (0, 1)]
            ExpectedVisited = [(0, -1);(0, 0)] |> Set.ofList
        };
        {
            Name = "Drag right";
            Start = [(-1, 0); (0, 0)]
            Input = { Dir = Direction.Right; Count = 1 };
            ExpectedPos = [(0, 0); (1, 0)]
            ExpectedVisited = [(-1, 0); (0, 0)] |> Set.ofList
        };
        {
            Name = "Drag offaxis up";
            Start = [(3, 0); (4, 0)]
            Input = { Dir = Up; Count = 1 };
            ExpectedPos = [(3, 0); (4, 1)]
            ExpectedVisited = [(3, 0)] |> Set.ofList
        };
        {
            Name = "Drag align up";
            Start = [ (3, 0); (4, 1) ]
            Input = { Dir = Up; Count = 1 };
            ExpectedPos = [ (4, 1); (4, 2) ]
            ExpectedVisited = [(3, 0); (4, 1)] |> Set.ofList
        };
        {
            Name = "Reverse and cover";
            Start = [(0, 0); (1, 0)]
            Input = { Dir = Direction.Left; Count = 1 };
            ExpectedPos = origin
            ExpectedVisited = [(0, 0)] |> Set.ofList
        };
        {
            Name = "Diagonal to aligned";
            Start = [(0, 1); (1, 0)]
            Input = { Dir = Up; Count = 1 };
            ExpectedPos = [(0, 1); (1, 1)]
            ExpectedVisited = [(0, 1)] |> Set.ofList
        };
        {
            Name = "Top left -> left";
            Start = [ (4, 3); (3, 4) ];
            Input = { Dir = Direction.Left; Count = 1 };
            ExpectedPos = [ (3, 4); (2, 4) ];
            ExpectedVisited = [(4, 3); (3, 4)] |> Set.ofList
        };
        {
            Name = "Drag right 5";
            Start = [(-1, 0); (0, 0)]
            Input = { Dir = Direction.Right; Count = 5 };
            ExpectedPos = [(4, 0); (5, 0)]
            ExpectedVisited =
                [(-1, 0); (0, 0); (1, 0); (2, 0); (3, 0); (4, 0);]
                |> Set.ofList
        };
    ]
    |> List.map Array.singleton

[<Theory>]
[<MemberData(nameof(testCases))>]
let ``movement test`` testCase =
    let initialState = PuzzleState.create testCase.Start
    
    let result = runInstruction initialState testCase.Input 
    
    result.Pos |> should equal testCase.ExpectedPos
    result.Tails |> should equal testCase.ExpectedVisited
    
let sampleProblemText =
    [
        "R 4"
        "U 4"
        "L 3"
        "D 1"
        "R 4"
        "D 1"
        "L 5"
        "R 2"
    ]

[<Fact>]
let ``sample problem, part 1`` () =
    let result = part1Core sampleProblemText
    
    result |> should equal "13"

let sampleProblem2Text = [
    "R 5";
    "U 8";
    "L 8";
    "D 3";
    "R 17";
    "D 10";
    "L 25";
    "U 20";
]

[<Fact>]
let ``sample problem, part 2`` () =
    let result = part2Core sampleProblem2Text
    
    result |> should equal "36"
