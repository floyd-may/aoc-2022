module aoc_2022.Day12

open FSharpx.Collections

type Heightmap = {
    Grid: int[][]
    Start: int * int
    Goal: int * int
}

let toInt (c: char) = System.Convert.ToInt32(c)
let toHeight c = (toInt c) - (toInt 'a')

let fromHeight h =
    System.Convert.ToChar(h + toInt 'a')

let parseInput lines =
    
    let charGrid =
        lines
        |> List.map Array.ofSeq
        |> Array.ofList
        
    let mutable start = 0,0
    let mutable goal = 0,0
    for y, row in (charGrid |> Array.indexed) do
        for x, c in (row |> Array.indexed) do
            if c = 'S' then
                start <- x,y
                charGrid[y][x] <- 'a'
            if c = 'E' then
                goal <- x,y
                charGrid[y][x] <- 'z'
                
    let grid =
        charGrid
        |> Array.map (Array.map toHeight)
        
    {
        Start = start
        Goal = goal
        Grid = grid
    }
    
let rec reconstructPath current cameFrom path =
    let newPath = current :: path
    
    match Map.tryFind current cameFrom with
    | None -> newPath
    | Some x -> reconstructPath x cameFrom newPath
    
let printGrid tx grid =
    let transformedGrid =
        grid
        |> Seq.indexed
        |> Seq.map (fun (y, row) ->
            row
            |> Seq.indexed
            |> Seq.map (fun (x, _) -> tx (x,y))
        )
        |> Seq.map (fun x -> System.String.Join("", x))
        
    for row in transformedGrid do
        printfn $"%s{row}"

let findPath heightmap =
    let width = heightmap.Grid[0].Length
    let height = heightmap.Grid.Length
    
    let heuristic (x,y) =
        abs (x - fst heightmap.Goal)
        + abs (y - snd heightmap.Goal)
    let mutable openSet = Heap.ofSeq false [(heuristic heightmap.Start, heightmap.Start)]
    let mutable cameFrom = Map.empty
    let mutable gScores =
        Map.empty
        |> Map.add heightmap.Start 0
        
    let inGrid coord =
        let x, y = coord
        
        x < width && x >= 0 && y < height && y >= 0
        
    let areNeighbors current candidate =
        let x1, y1 = current
        let x2, y2 = candidate
        
        let currentHeight = heightmap.Grid[y1][x1]
        let candidateHeight = heightmap.Grid[y2][x2]
        
        candidateHeight - currentHeight <= 1
        
    let getNeighbors coord =
        let x,y = coord
        [
            (x + 1, y);
            (x - 1, y);
            (x, y + 1);
            (x, y - 1)
        ]
        |> List.filter inGrid
        |> List.filter (areNeighbors coord)
        
        
    let getGScore coord =
        gScores
        |> Map.tryFind coord
        |> Option.defaultValue System.Int32.MaxValue
        
    let mutable solution = None
    
    while openSet.Length > 0 && solution.IsNone do
        let (_, current), remainingOpenSet = openSet.Uncons()
        openSet <- remainingOpenSet
        
        if current = heightmap.Goal
        then
            solution <-
                reconstructPath current cameFrom []
                |> Some
        else
        
        let neighbors = getNeighbors current
        
        for neighbor in neighbors do
            // the + 1 here is the distance/weight. All neighbors are distance = 1
            let tentativeGScore = 1 + getGScore current
            let neighborGScore = getGScore neighbor
            if tentativeGScore < neighborGScore then
                cameFrom <-
                    cameFrom
                    |> Map.add neighbor current
                gScores <-
                    gScores
                    |> Map.add neighbor tentativeGScore
                let fScore = tentativeGScore + heuristic neighbor
                openSet <-
                    openSet
                    |> Heap.insert (fScore, neighbor)
    match solution with
    | Some x -> x.Length - 1
    | None ->
        
    let isVisited coord =
        gScores.ContainsKey(coord)
        
    let toVisitedLetter coord =
        let capitalize = isVisited coord
        let letter =
            heightmap.Grid[snd coord][fst coord]
            |> fromHeight
            |> string
        if capitalize
        then letter.ToUpper()
        else letter
        
        
    do printGrid toVisitedLetter heightmap.Grid
        
    do printfn $"searched %i{gScores.Count} of %i{width * height} possible values"
    failwith "solution not found"
    
let part1() =
    let lines =
        System.IO.File.ReadLines("./day12.txt")
        |> List.ofSeq
    
    let heightmap = parseInput lines
    
    let result = findPath heightmap
    
    result |> string
