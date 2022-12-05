module aoc_2022.Day3Tests
open Xunit
open FsUnit.Xunit


[<Theory>]
[<InlineData('a', 1)>]
[<InlineData('b', 2)>]
[<InlineData('A', 27)>]
[<InlineData('B', 28)>]
let itemPriority (input: char, expected: int) =
    let actual = Day3.getPriority input
    
    actual |> should equal expected
