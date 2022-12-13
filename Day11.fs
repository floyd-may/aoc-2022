module aoc_2022.Day11

open FParsec
open ParsingUtils
open Microsoft.FSharp.Core.Operators.Checked

type Monkey = {
    Id: int
    Items: int64 list
    Operation: int64 -> int64
    Divisor: int64
    IfFalse: int
    IfTrue: int
    InspectionCount: int64
}

module Monkey =
    let private parseOperation =
        let preamble = spaces >>. skipString "Operation: new = "
        
        let pOperand = 
            (pint64 |>> Some)
            <|> (skipString "old" >>. preturn None)
            
        let pOperation =
            tuple3 pOperand (spaces >>. anyOf ['*'; '+'] .>> spaces) pOperand
            
        preamble >>. pOperation
        
    let private monkeyParser =
        let parseId =
            skipString "Monkey "
            >>. pint32
            .>> skipRestOfLine true
        let parseStartingItems =
            spaces
            >>. skipString "Starting items: "
            >>. (sepBy pint64 (skipString ", "))
            .>> skipNewline
        let parseIf boolStr =
            spaces
            >>. skipString "If "
            >>. skipString boolStr
            >>. skipString ": throw to monkey "
            >>. pint32
        let parseTest =
            spaces
            >>. skipString "Test: divisible by "
            >>. pint64
            .>> skipNewline
        let parseIfTrue = parseIf "true"
        let parseIfFalse = parseIf "false"
            
        let parseTrueFalse =
            tuple2 (parseIfTrue .>> skipNewline) parseIfFalse
            
        let toMonkey (monkeyId, items, operation, divisor, (ifTrue, ifFalse)) =
            let reifyOperand operand =
                match operand with
                | None -> id
                | Some x -> (fun _ -> x)
            let left, operator, right = operation
            let leftArg = reifyOperand left
            let rightArg = reifyOperand right
            let operationFn =
                match operator with
                | '*' -> fun x -> leftArg(x) * rightArg(x)
                | '+' -> fun x -> leftArg(x) + rightArg(x)
                | _ -> failwith "whoops"
            
            {
                Id = monkeyId;
                Items = items;
                Operation = operationFn;
                Divisor = divisor
                IfTrue = ifTrue;
                IfFalse = ifFalse
                InspectionCount = 0
            }
            
        tuple5 parseId parseStartingItems parseOperation parseTest parseTrueFalse
        |>> toMonkey

    let parse x =
        runParser monkeyParser x
        
    let parseMany text =
        let pDoubleNewline = skipNewline >>. skipNewline
        let p = (sepBy1 monkeyParser pDoubleNewline) .>> eof
        
        let monkeys =
            runParser p text
            |> List.map (fun x -> (x.Id, x))
            |> Map.ofList
            
        let divisorProduct =
            monkeys
            |> Map.values
            |> Seq.map (fun x -> x.Divisor)
            |> Seq.reduce (fun x y -> x * y)
            
        let wrapOperation op =
            fun x -> op (x % divisorProduct)
        monkeys
        |> Map.map (fun k m -> { m with Operation = wrapOperation m.Operation })
        
    let private getDest item m =
        if item % m.Divisor = 0L
        then m.IfTrue
        else m.IfFalse
        
    let cycle worryDivisor m =
        let destinations =
            m.Items
            |> List.map m.Operation
            |> List.map (fun x -> x / worryDivisor) // monkey gets bored
            |> List.map (fun x -> (getDest x m, x))
            
        let newMonkey = {
            m with
                Items = [];
                InspectionCount = m.InspectionCount + (m.Items.Length |> int64)
        }
        
        (newMonkey, destinations)

    let addItem item monkey =
        {
            monkey with Items = monkey.Items @ [item]
        }

let cycleAndMerge monkeyId worryDivisor (monkeys: Map<int, Monkey>) =
    let monkey = monkeys[monkeyId]
    
    let (newMonkey, destinations) = Monkey.cycle worryDivisor monkey
    
    let mutable newMonkeys = monkeys
    
    for (otherId, item) in destinations do
        let otherMonkey =
            newMonkeys[otherId]
            |> Monkey.addItem item
    
        newMonkeys <-
            newMonkeys
            |> Map.add otherId otherMonkey
            
    newMonkeys
    |> Map.add newMonkey.Id newMonkey

let runCore worryDivisor numberOfRounds text =
    let mutable monkeys = Monkey.parseMany text
    let orderedKeys =
        monkeys
        |> Map.keys
        |> Seq.sort
        |> List.ofSeq
        
    for _ in 1..numberOfRounds do
        for monkeyId in orderedKeys do
            monkeys <- cycleAndMerge monkeyId worryDivisor monkeys
        
    (*
    monkeys
    |> Map.values
    |> Seq.map (fun x -> sprintf $"Monkey %i{x.Id} inspected items %i{x.InspectionCount} times")
    |> fun x -> System.String.Join("\n", x)
    |> printfn "%s"
    *)
    
    monkeys
    |> Map.values
    |> Seq.map (fun x -> x.InspectionCount)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (fun x y -> x * y)
    
let part1core text =
    runCore 3L 20 text

let private readFile() =
    let lines = System.IO.File.ReadAllLines "./day11.txt"
    
    System.String.Join("\n", lines)

let part1 () =
    let fileText = readFile()
    
    part1core fileText
    |> string
    
let part2 () =
    let fileText = readFile()
    
    runCore 1L 10000 fileText
    |> string
