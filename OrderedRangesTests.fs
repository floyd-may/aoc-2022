module aoc_2022.OrderedRangesTests

open Xunit
open FsUnit.Xunit
open OrderedRanges

[<Fact>]
let ``add base case`` () =
    let r = { Start = 1; End = 4; }
    let result = OrderedRanges.add r []
    
    result |> should equal [r]

[<Fact>]
let ``insert before`` () =
    let rleft = { Start = 1; End = 4; }
    let rright = { Start = 6; End = 8; }
    
    let result = OrderedRanges.add rleft [rright]
    
    result |> should equal [rleft; rright]
    
[<Fact>]
let ``insert middle`` () =
    let left = { Start = 1; End = 4; }
    let middle = { Start = 6; End = 8; }
    let right = { Start = 10; End = 15; }
    
    let result = OrderedRanges.add middle [left; right]
    
    result |> should equal [left; middle; right]
    
[<Fact>]
let ``insert end`` () =
    let left = { Start = 1; End = 4; }
    let middle = { Start = 6; End = 8; }
    let right = { Start = 10; End = 15; }
    
    let result = OrderedRanges.add right [left; middle]
    
    result |> should equal [left; middle; right]


[<Fact>]
let ``merge extend left``() =
    let farLeft = { Start = -10; End = -5; }
    let left = { Start = 1; End = 4; }
    let middle = { Start = 5; End = 8; }
    let right = { Start = 10; End = 15; }
    
    let result = OrderedRanges.add middle [farLeft; left; right]
    
    let newLeft = { Start = 1; End = 8 }
    
    result |> should equal [farLeft; newLeft; right]
    
[<Fact>]
let ``merge insert left``() =
    let left = { Start = 1; End = 4; }
    let middle = { Start = 6; End = 9; }
    let right = { Start = 10; End = 15; }
    
    let result = OrderedRanges.add middle [left; right]
    
    let newRight = { Start = 6; End = 15 }
    
    result |> should equal [left; newRight]
    
[<Fact>]
let ``swallow left and right``() =
    let left = { Start = 1; End = 4; }
    let middle = { Start = 5; End = 9; }
    let right = { Start = 10; End = 15; }
    
    let result = OrderedRanges.add middle [left; right]
    
    let expected = [{ Start = 1; End = 15 }]
    
    result |> should equal expected
