module aoc_2022.Day5Tests

open System
open Xunit
open FsUnit.Xunit
open FParsec
open aoc_2022.Day5

[<Fact>]
let ``pivots crates``() =
    let crates = (
        [
            [None;      Some "D"; None];
            [Some "N";  Some "C"; None];
            [Some "Z";  Some "M"; Some "P"]
        ],
        [1; 2; 3]
    )
    
    let expected =
        [
            (1, ["N"; "Z"]);
            (2, ["D"; "C"; "M"]);
            (3, ["P"])
        ]
        |> Map.ofList
    
    pivotCrates crates |> should equal expected

[<Fact>]
let ``parses file``() =
    let expected = (
        [
            (1, ["N"; "Z"]);
            (2, ["D"; "C"; "M"]);
            (3, ["P"])
        ]
        |> Map.ofList,
        [
            { Count = 1; From = 2; To = 1 };
            { Count = 3; From = 1; To = 3 };
            { Count = 2; From = 2; To = 1 };
            { Count = 1; From = 1; To = 2 };
        ]
    )
    
    let fileContents = String.Join("\n", [
        "    [D]    ";
        "[N] [C]    ";
        "[Z] [M] [P]";
        " 1   2   3 ";
        "";
        "move 1 from 2 to 1";
        "move 3 from 1 to 3";
        "move 2 from 2 to 1";
        "move 1 from 1 to 2";
    ])
    
    let actual =
        match run parseAll fileContents with
        | Success (x, _, _) -> x
        | Failure (msg, _, _) -> failwithf $"parse fail: %s{msg}"
    
    actual |> should equal expected
