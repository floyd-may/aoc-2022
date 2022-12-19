module aoc_2022.Day15Tests

open Xunit
open FsUnit.Xunit

open Day15
open OrderedRanges

[<Fact>]
let ``excluded positions for row``() =
    let sensor = {
        Loc = (8, 7)
        Beacon = 2, 10
    }
    
    let result =
        Sensor.nonBeaconLocationsForRow sensor 12
    
    result |> should equal [
        { Start = 4; End = 12 }
    ]
    
[<Fact>]
let ``excluded positions for row with beacon``() =
    let sensor = {
        Loc = (8, 7)
        Beacon = 2, 10
    }
    
    let result =
        Sensor.nonBeaconLocationsForRow sensor 10
    
    result |> should equal [
        { Start = 3; End = 14 }
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
    
[<Fact>]
let ``part 2 sample input``() =
    let result = part2core sampleInput 20
    
    result |> should equal 56000011

[<Fact>]
let ``part2 merge ranges row 0``() =
    let sensors =
        sampleInput
        |> List.map Parsing.parseLine
        
    let result =
        sensors
        |> Seq.map (fun x -> Sensor.excludedLocationsForRow x 0)
        |> Seq.collect Option.toList
        |> Seq.fold (fun rs r -> OrderedRanges.add r rs) []
        
    result |> should equal [{ Start = -8; End = 26 }]
