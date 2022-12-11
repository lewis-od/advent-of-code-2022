module AoC22.Day11.Solution

open Microsoft.FSharp.Core

module Monkey =
    type T =
        { Id: int
          Items: uint64 list
          Operation: uint64 -> uint64
          Test: uint64
          IfTrue: int
          IfFalse: int
          NumInspected: uint64 }
        
    type Monkeys = Map<int, T>
    
    let create num items operation test (ifTrue, ifFalse) =
        { Id = num
          Items = items
          Operation = operation
          Test = test
          IfTrue = ifTrue
          IfFalse = ifFalse
          NumInspected = (uint64 0) }
    
    let receiveItem item monkey =
        let newItems = monkey.Items @ [item]
        { monkey with Items = newItems  }
    
    let takeItem monkey =
        match monkey.Items with
        | item::items -> (Some(item), {monkey with Items=items})
        | [] -> (None, monkey)
    
    let private parseOperation (left: string) (operator: string) (right: string): uint64 -> uint64 =
        let leftFn =
            if left = "old" then id
            else (fun _ -> uint64 left)
        let rightFn =
            if right = "old" then id
            else (fun _ -> uint64 right)
        
        let performOperation =
            if operator = "+" then (+)
            elif operator = "*" then (*)
            else failwith $"Unknown operator {operator}"
        
        fun old -> performOperation (leftFn old) (rightFn old)
    
    let parse (rows: string[]): T =
        let parseRow monkey (row: string) =
            let trimmedRow = row.TrimStart()
            if trimmedRow.StartsWith("Starting items:") then
                let items =
                    trimmedRow.Replace("Starting items: ", "").Split(", ")
                    |> Array.toList
                    |> List.map uint64
                {monkey with Items=items}
            elif trimmedRow.StartsWith("Operation:") then
                let parts = trimmedRow.Replace("Operation: new = ", "").Split(" ")
                let operation = parseOperation parts[0] parts[1] parts[2]
                {monkey with Operation=operation}
            elif trimmedRow.StartsWith("Test: divisible by ") then
                let divisor = trimmedRow.Replace("Test: divisible by ", "") |> uint64
                {monkey with Test=divisor}
            elif trimmedRow.StartsWith("If true:") then
                let throwTo = row.Substring(29) |> int
                {monkey with IfTrue=throwTo}
            elif trimmedRow.StartsWith("If false:") then
                let throwTo = row.Substring(29) |> int
                {monkey with IfFalse=throwTo}
            elif row.StartsWith("Monkey") then
                let identifier = row.Substring(7, 1) |> int
                {monkey with Id=identifier}
            else failwith $"Unable to process row: '{row}'"
        
        let initialMonkey = create -1 [] id 0uL (0, 0)
        
        (initialMonkey, rows) ||> Seq.fold parseRow

    let private throwItem (reduceWorry: uint64 -> uint64) (monkeys: Monkeys) (monkeyId: int) (item: uint64) =
        let monkey = (monkeyId, monkeys) ||> Map.find
        let updatedWorry = item |> (monkey.Operation >> reduceWorry)
        let throwTo =
            if (updatedWorry % monkey.Test = 0uL) then monkey.IfTrue
            else monkey.IfFalse
        monkeys
        |> Map.change throwTo (fun possibleMonkey ->
            match possibleMonkey with
            | Some toThrowTo -> Some(receiveItem updatedWorry toThrowTo)
            | None -> failwith $"Unable to find monkey {throwTo} to throw to")
        |> Map.change monkeyId (fun maybeMonkey ->
            match maybeMonkey with
            | Some monkey -> Some({monkey with NumInspected=(monkey.NumInspected + 1uL)})
            | None -> failwith "Error")

    let rec throwAllItems (reduceWorry: uint64 -> uint64) (monkeys: Monkeys) (monkeyId: int) =
        let item, updatedMonkey = (monkeyId, monkeys) ||> Map.find |> takeItem
        let updatedMonkeys = monkeys |> Map.change monkeyId (fun _ -> Some(updatedMonkey))
        match item with
        | Some value -> throwAllItems reduceWorry (throwItem reduceWorry updatedMonkeys monkeyId value) monkeyId
        | None -> monkeys

let parseAllMonkeys (input: string seq): Monkey.Monkeys =
    input
    |> Seq.filter ((<>) "")
    |> Seq.chunkBySize 6
    |> Seq.map Monkey.parse
    |> Seq.fold (fun monkeys monkey -> (monkey, monkeys) ||> Map.add monkey.Id) (Map [])

let performRound (reduceWorry: uint64 -> uint64) (monkeys: Monkey.Monkeys) =
    monkeys
    |> Map.keys
    |> Seq.sort
    |> Seq.fold (Monkey.throwAllItems reduceWorry) monkeys

let performNRounds (n: int) (reduceWorry: uint64 -> uint64) (monkeys: Monkey.Monkeys) =
    Seq.init n id
    |> Seq.fold (fun monks _ -> performRound reduceWorry monks) monkeys

let calcMonkeyBusiness (monkeys: Monkey.Monkeys) =
    monkeys
    |> Map.values
    |> Seq.map (fun monk -> monk.NumInspected)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

let divideBy3AndFloor x = x / 3uL

let part1 () =
    let business =
        System.IO.File.ReadLines "Day11/input.txt"
        |> parseAllMonkeys
        |> performNRounds 20 divideBy3AndFloor
        |> calcMonkeyBusiness
    printfn $"Monkey business: {business}"

let rec gcd x y = if y = 0uL then x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)
    
let shrinkModLcm (monkeys: Monkey.Monkeys) =
    let lcmOfTests =
        monkeys
        |> Map.values
        |> Seq.map (fun m -> m.Test)
        |> Seq.reduce lcm

    fun worry -> worry % lcmOfTests

let performRoundsModReduction (numRounds: int) (input: string seq) =
    let monkeys = input |> parseAllMonkeys
    let reduceWorry = shrinkModLcm monkeys
    (reduceWorry, monkeys)
    ||> performNRounds numRounds

let part2 () =
    let business =
        System.IO.File.ReadLines "Day11/input.txt"
        |> performRoundsModReduction 10_000
        |> calcMonkeyBusiness
    printfn $"Monkey business: {business}"
