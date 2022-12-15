module aoc_2022.Day12

open FSharpx.Collections

type Heightmap = {
    Grid: int[][]
    Start: int * int
    Goal: int * int
}

module Elevation =
    let private toInt (c: char) = System.Convert.ToInt32(c)
    let fromChar c = (toInt c) - (toInt 'a')

    let toChar h =
        System.Convert.ToChar(h + toInt 'a')
        
type Coord = int * int

module Coord =
    let distance a b =
        let ax, ay = a
        let bx, by = b
        
        (abs (ax - bx)) + (abs (ay - by))
        
module AStar =
    type AStarState = {
        Grid: int[][]
        Heuristic: Coord -> int
        OpenSet: Heap<int * Coord>
        CameFrom: Map<Coord, Coord>
        GScores: Map<Coord, int>
        GoalCriteria: Coord -> bool
        NeighborCriteria: Coord -> Coord -> bool
    }
    
    let init start grid heuristic goalCriteria neighborCriteria =
        {
            Grid = grid
            Heuristic = heuristic
            GoalCriteria = goalCriteria
            NeighborCriteria = neighborCriteria
            CameFrom = Map.empty
            GScores =
                Map.empty
                |> Map.add start 0
            OpenSet =
                Heap.empty false
                |> Heap.insert (0,start)
        }
        
    let private gridDimensions state =
        let width = state.Grid[0].Length
        let height = state.Grid.Length
        
        width,height
        
    let private inGrid state coord =
        let x, y = coord
        let width, height = gridDimensions state
        
        x < width && x >= 0 && y < height && y >= 0
        
    let private getNeighbors state coord =
        let x,y = coord
        [
            (x + 1, y);
            (x - 1, y);
            (x, y + 1);
            (x, y - 1)
        ]
        |> List.filter (inGrid state)
        |> List.filter (state.NeighborCriteria coord)
        
    let private getGScore state coord =
        state.GScores
        |> Map.tryFind coord
        |> Option.defaultValue System.Int32.MaxValue

    let rec private reconstructPath current cameFrom path =
        let newPath = current :: path
        
        match Map.tryFind current cameFrom with
        | None -> newPath
        | Some x -> reconstructPath x cameFrom newPath
        
    let private updateOpenSet state openSet =
        {
            state with OpenSet = openSet
        }
        
    let private updateNeighborCalc state openSet cameFrom gScores =
        {
            state with
                OpenSet = openSet
                CameFrom = cameFrom
                GScores = gScores
        }

    let findPath startState =
        let mutable solution = None
        
        let mutable state = startState
        
        while state.OpenSet.Length > 0 && solution.IsNone do
            let (_, current), remainingOpenSet = state.OpenSet.Uncons()
            state <- updateOpenSet state remainingOpenSet
            
            if state.GoalCriteria current
            then
                solution <-
                    reconstructPath current state.CameFrom []
                    |> Some
            else
            
            let neighbors = getNeighbors state current
            
            
            for neighbor in neighbors do
                // the + 1 here is the distance/weight. All neighbors are distance = 1
                let tentativeGScore = 1 + getGScore state current
                let neighborGScore = getGScore state neighbor
                if tentativeGScore < neighborGScore then
                    let cameFrom =
                        state.CameFrom
                        |> Map.add neighbor current
                    let gScores =
                        state.GScores
                        |> Map.add neighbor tentativeGScore
                    let fScore = tentativeGScore + state.Heuristic neighbor
                    let openSet =
                        state.OpenSet
                        |> Heap.insert (fScore, neighbor)
                    state <- updateNeighborCalc state openSet cameFrom gScores
        
        solution, state

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
        |> Array.map (Array.map Elevation.fromChar)
        
    {
        Start = start
        Goal = goal
        Grid = grid
    }
    
    
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
        
    System.String.Join("\n", transformedGrid)

let findPath heightmap =
    let goal = heightmap.Goal
    let heuristic coord = Coord.distance goal coord
    
    let goalCriteria x =
        x = goal
        
    let areNeighbors current candidate =
        let x1, y1 = current
        let x2, y2 = candidate
        
        let currentHeight = heightmap.Grid[y1][x1]
        let candidateHeight = heightmap.Grid[y2][x2]
        
        candidateHeight - currentHeight <= 1
        
    let state =
        AStar.init
            heightmap.Start
            heightmap.Grid
            heuristic
            goalCriteria
            areNeighbors
            
    let solution, endState = AStar.findPath state
    match solution with
    | Some x -> x.Length - 1
    | None ->
        
    let gScores = endState.GScores
    let isVisited coord =
        gScores.ContainsKey(coord)
        
    let toVisitedLetter coord =
        let capitalize = isVisited coord
        let letter =
            heightmap.Grid[snd coord][fst coord]
            |> Elevation.toChar
            |> string
        if capitalize
        then letter.ToUpper()
        else letter
        
    printfn $"%s{printGrid toVisitedLetter heightmap.Grid}"
        
    failwith "solution not found"
    
let part1() =
    let lines =
        System.IO.File.ReadLines("./day12.txt")
        |> List.ofSeq
    
    let heightmap = parseInput lines
    
    let result = findPath heightmap
    
    result |> string
    
let part2core lines =
    let heightmap = parseInput lines
    
    let aCoords =
        seq { 0..(heightmap.Grid[0].Length - 1) }
        |> Seq.collect (
            fun x ->
                seq { 0..(heightmap.Grid.Length - 1) }
                |> Seq.map (fun y -> x,y)
        )
        |> Seq.filter (fun coord -> heightmap.Grid[snd coord][fst coord] = 0)
        |> Set.ofSeq
        
    let heuristic coord =
        aCoords
        |> Seq.map (Coord.distance coord)
        |> Seq.sort
        |> Seq.head
        
    let successCriteria coord = aCoords |> Set.contains coord
    
    let areNeighbors current candidate =
        let x1, y1 = current
        let x2, y2 = candidate
        
        let currentHeight = heightmap.Grid[y1][x1]
        let candidateHeight = heightmap.Grid[y2][x2]
        
        currentHeight - candidateHeight <= 1
        
    let initialState =
        AStar.init
            heightmap.Goal
            heightmap.Grid
            heuristic
            successCriteria
            areNeighbors
            
    let result, endState = AStar.findPath initialState
    
    (*
    let gScores = endState.GScores
    let isVisited coord =
        gScores.ContainsKey(coord)
        
    let toVisitedLetter coord =
        let capitalize = isVisited coord
        let letter =
            heightmap.Grid[snd coord][fst coord]
            |> Elevation.toChar
            |> string
        if capitalize
        then letter.ToUpper()
        else letter
        
    printGrid toVisitedLetter heightmap.Grid
    *)
    
    match result with
    | None -> failwith "no solution found"
    | Some x -> x.Length - 1

let part2 () =
    let lines =
        System.IO.File.ReadLines("./day12.txt")
        |> List.ofSeq
        
    part2core lines |> string
