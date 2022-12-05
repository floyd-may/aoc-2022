module aoc_2022.Day4Tests

open aoc_2022.Day4
open Xunit
open FsUnit.Xunit


[<Theory>]
[<InlineData(1, 2, 3, 4, false)>]
[<InlineData(1, 2, 2, 4, true)>]
[<InlineData(10, 11, 1, 2, false)>]
[<InlineData(3, 4, 1, 3, true)>]
let overlap (xStart, xEnd, yStart, yEnd, expected: bool) =
    let x = { Start = xStart; End = xEnd }
    let y = { Start = yStart; End = yEnd }
    
    let actual = overlaps x y
    
    actual |> should equal expected
