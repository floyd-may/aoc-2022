module aoc_2022.Day10Tests

open Xunit
open FsUnit.Xunit

open Day10

[<Fact>]
let ``runInstr noop``() =
    let result =
        runInstr 6 Noop
        |> List.ofSeq
        
    result |> should equal [6]
    
[<Fact>]
let ``runInstr AddX``() =
    let result =
        runInstr 6 (AddX 3)
        
    result |> should equal [6;9]

[<Fact>]
let ``smaller sample program``() =
    let instructions = [
        Noop;
        AddX 3;
        AddX -5;
    ]
    
    let result =
        runProgram instructions
        |> List.ofSeq
    
    result |> should equal [
        (1,1);
        (2,1);
        (3,1);
        (4,4);
        (5,4);
        (6,-1);
    ]

let largerProgramExample = [
    "addx 15";
    "addx -11";
    "addx 6";
    "addx -3";
    "addx 5";
    "addx -1";
    "addx -8";
    "addx 13";
    "addx 4";
    "noop";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx -35";
    "addx 1";
    "addx 24";
    "addx -19";
    "addx 1";
    "addx 16";
    "addx -11";
    "noop";
    "noop";
    "addx 21";
    "addx -15";
    "noop";
    "noop";
    "addx -3";
    "addx 9";
    "addx 1";
    "addx -3";
    "addx 8";
    "addx 1";
    "addx 5";
    "noop";
    "noop";
    "noop";
    "noop";
    "noop";
    "addx -36";
    "noop";
    "addx 1";
    "addx 7";
    "noop";
    "noop";
    "noop";
    "addx 2";
    "addx 6";
    "noop";
    "noop";
    "noop";
    "noop";
    "noop";
    "addx 1";
    "noop";
    "noop";
    "addx 7";
    "addx 1";
    "noop";
    "addx -13";
    "addx 13";
    "addx 7";
    "noop";
    "addx 1";
    "addx -33";
    "noop";
    "noop";
    "noop";
    "addx 2";
    "noop";
    "noop";
    "noop";
    "addx 8";
    "noop";
    "addx -1";
    "addx 2";
    "addx 1";
    "noop";
    "addx 17";
    "addx -9";
    "addx 1";
    "addx 1";
    "addx -3";
    "addx 11";
    "noop";
    "noop";
    "addx 1";
    "noop";
    "addx 1";
    "noop";
    "noop";
    "addx -13";
    "addx -19";
    "addx 1";
    "addx 3";
    "addx 26";
    "addx -30";
    "addx 12";
    "addx -1";
    "addx 3";
    "addx 1";
    "noop";
    "noop";
    "noop";
    "addx -9";
    "addx 18";
    "addx 1";
    "addx 2";
    "noop";
    "noop";
    "addx 9";
    "noop";
    "noop";
    "noop";
    "addx -1";
    "addx 2";
    "addx -37";
    "addx 1";
    "addx 3";
    "noop";
    "addx 15";
    "addx -21";
    "addx 22";
    "addx -6";
    "addx 1";
    "noop";
    "addx 2";
    "addx 1";
    "noop";
    "addx -10";
    "noop";
    "noop";
    "addx 20";
    "addx 1";
    "addx 2";
    "addx 2";
    "addx -6";
    "addx -11";
    "noop";
    "noop";
    "noop";
]
    
[<Fact>]
let ``larger sample program``() =
    let result = part1core largerProgramExample
    
    result |> should equal 13140

[<Fact>]
let ``part2 sample`` () =
    let expected = [
        "##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ";
        "###   ###   ###   ###   ###   ###   ### ";
        "####    ####    ####    ####    ####    ";
        "#####     #####     #####     #####     ";
        "######      ######      ######      ####";
        "#######       #######       #######     ";
    ]
    
    let actual = part2core largerProgramExample
    
    actual |> should equal expected
