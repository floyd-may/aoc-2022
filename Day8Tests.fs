module aoc_2022.Day8Tests

open Xunit
open FsUnit.Xunit

let grid = [|
    [|3;0;3;7;3|];
    [|2;5;5;1;2|];
    [|6;5;3;3;2|];
    [|3;3;5;4;9|];
    [|3;5;3;9;0|];
|]
    
[<Fact>]
let ``part2 sample`` () =
    Day8.part2core grid |> should equal "8"
    
[<Fact>]
let ``view distance``() =
    Day8.P2.viewDistance 5 [3;5] |> should equal 2

[<Fact>]
let ``viewVectors`` () =
    Day8.P2.viewVectors grid (2,3)
    |> List.map List.ofSeq
        |> should equal [
            [3;5;3]
            [3]
            [3;3]
            [4;9]
        ]
