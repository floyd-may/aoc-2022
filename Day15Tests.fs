module aoc_2022.Day15Tests

open Xunit
open FsUnit.Xunit

open Day15

[<Fact>]
let ``excluded positions for row``() =
    let sensor = {
        Loc = (8, 7)
        Beacon = 2, 10
    }
    
    let result =
        Sensor.excludedPositionsForRow sensor 12
        |> List.ofSeq
    
    result |> should equal [
        (4, 12);
        (5, 12);
        (6, 12);
        (7, 12);
        (8, 12);
        (9, 12);
        (10, 12);
        (11, 12);
        (12, 12);
    ]

let sampleInput = [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16";
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3";
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16";
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16";
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16";
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10";
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10";
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10";
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17";
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22";
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3";
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3";
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3";
]

[<Fact>]
let ``part 1 sample input``() =
    let result = part1core sampleInput 10
    
    result |> should equal 26
