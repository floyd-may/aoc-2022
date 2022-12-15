module aoc_2022.Day12Tests

open Xunit
open FsUnit.Xunit
open Day12
let sampleInput =
    [
        "Sabqponm";
        "abcryxxl";
        "accszExk";
        "acctuvwj";
        "abdefghi";
    ]
    
[<Fact>]
let ``input parsing`` () =
    let result = parseInput sampleInput
    
    result |> should equal {
        Start = 0,0;
        Goal = 5,2
        Grid = [|
            [| 0; 0; 1; 16; 15; 14; 13; 12 |]
            [| 0; 1; 2; 17; 24; 23; 23; 11 |]
            [| 0; 2; 2; 18; 25; 25; 23; 10 |]
            [| 0; 2; 2; 19; 20; 21; 22; 9 |]
            [| 0; 1; 3;  4;  5;  6;  7; 8 |]
        |]
    }

[<Fact>]
let ``path finding`` () =
    let startState = parseInput sampleInput
    
    let pathLength = findPath startState
    
    pathLength |> should equal 31
