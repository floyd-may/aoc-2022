module aoc_2022.Day13Tests

open Xunit
open FsUnit.Xunit
open Day13

let sampleInput = [
    "[1,1,3,1,1]";
    "[1,1,5,1,1]"
    "";
    "[[1],[2,3,4]]";
    "[[1],4]";
    "";
    "[9]";
    "[[8,7,6]]";
    "";
    "[[4,4],4,4]";
    "[[4,4],4,4,4]";
    "";
    "[7,7,7,7]";
    "[7,7,7]";
    "";
    "[]";
    "[3]";
    "";
    "[[[]]]";
    "[[]]";
    "";
    "[1,[2,[3,[4,[5,6,7]]]],8,9]";
    "[1,[2,[3,[4,[5,6,0]]]],8,9]";
]

[<Fact>]
let ``sample input, part 1``() =
    let result = part1core sampleInput
    
    result |> should equal 13

[<Fact>]
let ``sample input, packetwise`` () =
    let packets = Parsing.parseInput (System.String.Join("\n", sampleInput))
    
    let result =
        packets
        |> Seq.map comparePackets
        |> List.ofSeq
        
    result |> should equal [-1; -1; 1; -1; 1; -1; 1; 1]
