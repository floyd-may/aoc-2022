module aoc_2022.Day14

open FParsec
open ParsingUtils

type Coord = int * int

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
    
let rec getSandPath usedCoords yMax path coord =
    if snd coord = yMax then Result.Error (coord :: path)
    else
        
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
    | None -> Result.Ok (coord :: path)
    | Some next -> getSandPath usedCoords yMax (coord :: path) next
    
let rec getRestingPlace usedCoords yMax coord =
    let path = getSandPath usedCoords yMax [] coord
        
    match path with
    | Result.Ok (res :: _) -> Some res
    | _ -> None
    
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
    
    while not stop do
        let restingPlace = getRestingPlace usedCoords yMax (500,0)
        
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
        
    let floor =
        ((-5000,yMax + 2),(5000,yMax + 2))
        |> pairToLine
        
    usedCoords <-
        floor
        |> Seq.fold (fun acc x -> Set.add x acc) usedCoords

    let rockCoordCount = Set.count usedCoords
    let mutable stop = false
    
    while not stop do
        let restingPlace = getRestingPlace usedCoords (yMax + 2) (500,0)
        
        match restingPlace with
        | None ->
            do stop <- true
        | Some coord ->
            do usedCoords <-
                usedCoords
                |> Set.add coord
                
    (Set.count usedCoords) - rockCoordCount
