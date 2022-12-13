module aoc_2022.Day11Tests

open Xunit
open FsUnit.Xunit
open Day11

let trim (x: string) = x.Trim()

let monkey0Text =
  """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3"""
  |> trim
  
let monkey2Text =
  """
Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3"""
  |> trim

let sampleText =
  """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
  """
  |> trim

[<Fact>]
let ``parse monkey 0`` () =
  let actual = Monkey.parse monkey0Text
  
  actual.Id |> should equal 0
  actual.Items |> should equal [79L;98L]
  actual.IfTrue |> should equal 2
  actual.IfFalse |> should equal 3
  
  actual.Operation 2 |> should equal 38L
  actual.Divisor |> should equal 23L
  
[<Fact>]
let ``parse monkey 2`` () =
  let actual = Monkey.parse monkey2Text
  
  actual.Id |> should equal 2
  actual.Items |> should equal [79L; 60L; 97L]
  actual.IfTrue |> should equal 1
  actual.IfFalse |> should equal 3
  
  actual.Operation 7 |> should equal 49L
  actual.Divisor |> should equal 13L

[<Fact>]
let ``cycle monkey 0`` () =
  let monkey = Monkey.parse monkey0Text
  
  let resultingMonkey, destinations = Monkey.cycle 3L monkey
  
  resultingMonkey.Items |> should be Empty
  resultingMonkey.InspectionCount |> should equal 2L
  destinations |> should equal [
    (3, 500L);
    (3, 620L);
  ]

[<Fact>]
let ``cycleAndMerge monkey 0``() =
  let monkeys = Monkey.parseMany sampleText
  
  let result = cycleAndMerge 0 3L monkeys
  
  result[0].Items |> should be Empty
  result[3].Items |> should equal (
    monkeys[3].Items @ [500L; 620L]
    )

[<Fact>]
let ``part1 sample``() =
  let result = part1core sampleText
  
  result |> should equal 10605L
