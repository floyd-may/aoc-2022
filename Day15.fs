module aoc_2022.Day15

type Coord = int * int

let distance a b =
    let x1,y1 = a
    let x2,y2 = b
    
    (abs (x1 - x2))
    +
    (abs (y1 - y2))
    
type Range = {
    Start: int
    End: int
}

type OrderedRanges = Range list
module OrderedRanges =
    let rec add r rs =
        match rs with
        | [] -> [r]
        | _ ->
        let left, right =
            rs
            |> List.partition (fun x -> x.End < r.Start)
        
        left @ (r :: right)

    
type Sensor = {
    Loc: Coord
    Beacon: Coord
}

module Sensor =
    let excludedPositionsForRow sensor row =
        let dist = distance sensor.Loc sensor.Beacon
        let x,y = sensor.Loc
        let rowDist = abs (y - row)
        let xRange = (dist - rowDist)
        
        let xStart = x - xRange
        let xEnd = x + xRange
        
        seq { xStart..xEnd }
        |> Seq.map (fun rangeX -> (rangeX, row))


module Parsing =
    open FParsec
    open ParsingUtils
    let parseLine line =
        let pCoord =
            tuple2
                (skipString "x=" >>. pint32)
                (skipString ", y=" >>. pint32)
            
        let pLine =
            tuple2
                (skipString "Sensor at " >>. pCoord)
                (skipString ": closest beacon is at " >>. pCoord)
            
        let result = runParser pLine line
        
        {
            Loc = fst result
            Beacon = snd result
        }

let part1core input target =
    let sensors =
        input
        |> List.map Parsing.parseLine
        
        
    let beacons =
        sensors
        |> List.map (fun x -> x.Beacon)
        |> Set.ofSeq
        
    let beaconExists x = Set.contains x beacons
    
    sensors
    |> Seq.collect (fun x -> Sensor.excludedPositionsForRow x target)
    |> Seq.filter (beaconExists >> not)
    |> Seq.distinct
    |> Seq.length

let part1 () =
    let lines =
        System.IO.File.ReadAllLines("./day15.txt")
        |> List.ofSeq
        
    part1core lines 2000000
    |> string
