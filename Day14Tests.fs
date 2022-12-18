module aoc_2022.Day14Tests

open Xunit
open FsUnit.Xunit
open Day14
let sampleInput = [
    "498,4 -> 498,6 -> 496,6";
    "503,4 -> 502,4 -> 502,9 -> 494,9"
]

[<Fact>]
let ``sample input, part 1`` () =
    let result = part1core sampleInput
    
    result |> should equal 24
    
[<Fact>]
let ``sample input, part 2`` () =
    let result = part2core sampleInput
    
    result |> should equal 93
    
[<Fact>]
let ``parse line``() =
    let result =
        parseLine sampleInput.Head
        |> Set.ofList
    
    let expected =
        [
            (498,4);
            (498,5);
            (498,6);
            (497,6);
            (496,6);
        ]
        |> Set.ofList
    
    result |> should equal expected

[<Fact>]
let ``pairToLine positive y`` () =
    let result =
        pairToLine ((498,4), (498,6))
        |> List.ofSeq
    
    result |> should equal [(498,4); (498,5); (498,6)]
