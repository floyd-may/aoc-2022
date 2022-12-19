module aoc_2022.Day14

open FParsec
open ParsingUtils

type Coord = int * int

type RestingPlaceResult =
    | Resting
    | FallingIntoVoid
    | Indeterminate

let pairToLine pair: Coord seq =
    let (x1, y1), (x2, y2) = pair
    
    if x1 <> x2
    then
        let xMin = min x1 x2
        let xMax = max x1 x2
        seq {
            for x in xMin..xMax do
                yield x,y1
        }
    else
        let yMin = min y1 y2
        let yMax = max y1 y2
        seq {
            for y in yMin..yMax do
                yield x1, y
        }

let parseLine line: Coord list =
    let pCoord =
        tuple2
            (pint32 .>> skipChar ',')
            pint32
        
    let pLine = (sepBy1 pCoord (skipString " -> ")) .>> eof
    
    let result = runParser pLine line
    
    result
    |> Seq.pairwise
    |> Seq.collect pairToLine
    |> List.ofSeq
    
let rec getRestingPlace usedCoords baseCase coord: Coord option =
    match baseCase coord with
    | Resting -> Some coord
    | FallingIntoVoid -> None
    | _ ->
        
    let x,y = coord
    let candidates =
        [
            (x, y + 1);
            (x - 1, y + 1);
            (x + 1, y + 1);
        ]
        
    let isUsed x = Set.contains x usedCoords
        
    let nextLoc =
        candidates
        |> List.tryFind (isUsed >> not)
    
    match nextLoc with
    | None -> Some coord
    | Some next -> getRestingPlace usedCoords baseCase next
    
let part1core input =
    let mutable usedCoords =
        input
        |> Seq.collect parseLine
        |> Set.ofSeq
        
    let rockCoordCount = Set.count usedCoords
        
    let yMax =
        usedCoords
        |> Seq.map snd
        |> Seq.max

    let mutable stop = false
    
    let baseCase coord =
        if snd coord = yMax
        then FallingIntoVoid
        else Indeterminate
        
    while not stop do
        let restingPlace = getRestingPlace usedCoords baseCase (500,0)
        
        match restingPlace with
        | None ->
            do stop <- true
        | Some coord ->
            do usedCoords <-
                usedCoords
                |> Set.add coord
                
    (Set.count usedCoords) - rockCoordCount


let part1 () =
    let lines =
        System.IO.File.ReadAllLines("./day14.txt")
        |> List.ofSeq
        
    part1core lines
    |> string
    
let part2core input =
    let mutable usedCoords =
        input
        |> Seq.collect parseLine
        |> Set.ofSeq
        
    let yMax =
        usedCoords
        |> Seq.map snd
        |> Seq.max

    let rockCoordCount = Set.count usedCoords
    let mutable stop = false
    
    let baseCase coord =
        if snd coord = yMax + 1
        then Resting
        else Indeterminate
    
    while not stop do
        let restingPlace = getRestingPlace usedCoords baseCase (500,0)
        
        match restingPlace with
        | None ->
            failwith "unexpected falling into void case"
        | Some coord ->
            do usedCoords <-
                usedCoords
                |> Set.add coord
            if coord = (500,0) then
                do stop <- true
                
    (Set.count usedCoords) - rockCoordCount
    
let part2 () =
    let lines =
        System.IO.File.ReadAllLines("./day14.txt")
        |> List.ofSeq
        
    part2core lines
    |> string
