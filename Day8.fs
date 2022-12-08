module aoc_2022.Day8

open System.IO

type TreeState =
    val mutable MaxLeft: int;
    val mutable MaxRight: int;
    val mutable MaxTop: int;
    val mutable MaxBottom: int;
    val Height: int;
    
    new(h) = {
        Height = h
        MaxLeft = -1
        MaxRight = -1
        MaxTop = -1
        MaxBottom = -1
    }

let private parseFile () =
    let lines = File.ReadAllLines "./day8.txt"
    
    let parseLine l =
        l
        |> Seq.map (fun x -> $"%c{x}")
        |> Seq.map System.Int32.Parse
        |> Array.ofSeq
        
    lines
    |> Seq.map parseLine
    |> Array.ofSeq
    
let private parsePart1 () =
    let grid = parseFile()
    grid
    |> Array.map (Array.map TreeState)

let part1 () =
    let state = parsePart1()
    
    let height = state.Length
    let width = state[0].Length
    
    for x in 1..(width - 1) do
        for y in 1..(height - 1) do
            let row = state[y]
            let upperRow = state[y - 1]
            let cell = row[x]
            let leftCell = row[x - 1]
            let topCell = upperRow[x]
            
            cell.MaxLeft <- max leftCell.MaxLeft leftCell.Height
            cell.MaxTop <- max topCell.MaxTop topCell.Height
            
    for x in seq { (width - 2) .. -1 .. 0 } do
        for y in seq { (height - 2) .. -1 .. 0 } do
            let row = state[y]
            let lowerRow = state[y + 1]
            let cell = row[x]
            let rightCell = row[x + 1]
            let bottomCell = lowerRow[x]
            
            cell.MaxRight <- max rightCell.MaxRight rightCell.Height
            cell.MaxBottom <- max bottomCell.MaxBottom bottomCell.Height

    let isVisible (c: TreeState) =
        let cellMin =
            min c.MaxRight c.MaxLeft
            |> min c.MaxTop
            |> min c.MaxBottom
            
        c.Height > cellMin

    Seq.collect id state
    |> Seq.filter isVisible
    |> Seq.length
    |> string
    
type CellPart2 = {
    Height: int
    X: int
    Y: int
}
    
module P2 =
    let viewDistance height vec =
        
        seq {
            let mutable keepGoing = true
            yield!
                vec
                |> Seq.takeWhile (
                    fun x ->
                        if not keepGoing
                            then false
                        else
                            if x >= height
                                then do keepGoing <- false

                            true
                )
            
        }
        |> Seq.length
        
    let cellFromCoord (rawGrid: int[][]) coord =
        let x,y = coord
        if x < 0 then failwith "x < 0"
        if y < 0 then failwith "y < 0"
        let row = rawGrid[snd coord]
        row[fst coord]
        
    let viewVectors (rawGrid: int[][]) coord =
        let height = rawGrid.Length
        let width = rawGrid[0].Length
        let cellX,cellY = coord
        
        let up =
            seq { (cellY - 1) .. -1 .. 0 }
            |> Seq.map (fun y -> cellX, y)
        let down =
            seq { (cellY + 1) .. (height - 1) }
            |> Seq.map (fun y -> cellX, y)
        let left =
            seq { (cellX - 1) .. -1 .. 0 }
            |> Seq.map (fun x -> x, cellY)
        let right =
            seq { (cellX + 1) .. (width - 1) }
            |> Seq.map (fun x -> x, cellY)
            
        [ up; down; left; right; ]
        |> List.map (Seq.map (cellFromCoord rawGrid))
        
let part2core (rawGrid:int[][]) =
    
    let cellsWithCoords =
        rawGrid
        |> Seq.map Seq.indexed
        |> Seq.indexed
        |> Seq.collect (
            fun (y, row) ->
                row
                |> Seq.map (fun (x, height) -> {
                    Height = height
                    X = x
                    Y = y
                })
            )
        
    let viewVectors = P2.viewVectors rawGrid
        
    let scenicScore cell =
        let vectors = viewVectors (cell.X, cell.Y)
        
        let distances =
            vectors
            |> List.map (P2.viewDistance cell.Height)
            
        List.reduce (fun a b -> a * b) distances
        
    let maxScore =
        cellsWithCoords
        |> Seq.map scenicScore
        |> Seq.max
        
    maxScore |> string
    
let part2 () =
    let rawGrid = parseFile()
    
    part2core rawGrid
