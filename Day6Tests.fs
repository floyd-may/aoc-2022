module aoc_2022.Day6Tests

open Xunit
open FsUnit.Xunit

[<Theory>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
let ``finds start of packet``(input, expected) =
    Day6.part1Core input |> should equal expected
