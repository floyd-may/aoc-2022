module aoc_2022.Day15

open OrderedRanges
type Coord = int * int

let distance a b =
    let x1,y1 = a
    let x2,y2 = b
    
    (abs (x1 - x2))
    +
    (abs (y1 - y2))
    
type Sensor = {
    Loc: Coord
    Beacon: Coord
}

module Sensor =
    let private excludedRowRange sensor row =
        let dist = distance sensor.Loc sensor.Beacon
        let x,y = sensor.Loc
        let rowDist = abs (y - row)
        let xRange = (dist - rowDist)
        
        let xStart = x - xRange
        let xEnd = x + xRange
        
        xStart, xEnd
        
    let nonBeaconLocationsForRow sensor row =
        let xStart, xEnd = excludedRowRange sensor row
        
        if xStart > xEnd then []
        else
        
        if snd sensor.Beacon = row
        then
            let xBeacon = fst sensor.Beacon
            [
                { Start = xStart; End = xBeacon - 1 };
                { Start = xBeacon + 1; End = xEnd };
            ]
            |> List.filter Range.valid
        else
            [{ Start = xStart; End = xEnd }]
            
    let excludedLocationsForRow sensor row = 
        let xStart, xEnd = excludedRowRange sensor row
        
        if xStart > xEnd
        then None
        else { Start = xStart; End = xEnd } |> Some


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
    
    sensors
    |> Seq.collect (fun x -> Sensor.nonBeaconLocationsForRow x target)
    |> Seq.fold (fun ranges r -> OrderedRanges.add r ranges) []
    |> Seq.map Range.size
    |> Seq.sum

let part1 () =
    let lines =
        System.IO.File.ReadAllLines("./day15.txt")
        |> List.ofSeq
        
    part1core lines 2000000
    |> string

let part2core input maxCoord =
    let sensors =
        input
        |> List.map Parsing.parseLine
        
    let getRowRanges r =
        sensors
        |> Seq.map (fun x -> Sensor.excludedLocationsForRow x r)
        |> Seq.collect Option.toList
        |> Seq.fold (fun rs r -> OrderedRanges.add r rs) []
        
    let tryGetBeaconLoc indexedRow =
        let y, ranges = indexedRow
        
        let isRelevant r =
            r.End > 0 || r.Start < maxCoord
        
        let filteredRanges =
            ranges
            |> List.filter isRelevant
            
        match filteredRanges with
        | [x] when x.Start = 1 -> (0, y) |> Some
        | [x] when x.End = maxCoord - 1 -> (maxCoord, y) |> Some
        | [left; right] -> (left.End + 1, y) |> Some
        | _ -> None
    
    let rows =
        seq { 0..maxCoord }
        |> Seq.map getRowRanges
        |> Seq.indexed
        
    let loc =
        rows
        |> Seq.map tryGetBeaconLoc
        |> Seq.find Option.isSome
        |> Option.get

    let solnX, solnY = loc
    
    (solnX |> int64) * 4000000L + (solnY |> int64)
    
let part2 () =
    let lines =
        System.IO.File.ReadAllLines("./day15.txt")
        |> List.ofSeq
        
    part2core lines 4000000
    |> string
