module AoC22.UnitTests.Day11.Day11Tests

open NUnit.Framework
open AoC22.Day11.Solution

[<TestFixture>]
type Day11Tests () =
    [<Test>]
    member this.``should parse monkey`` () =
        let rows = System.IO.File.ReadLines "Day11/monkey.txt" |> Seq.toArray
        let monkey = Monkey.parse rows
        
        Assert.That(monkey.Id, Is.EqualTo(0))
        Assert.That(monkey.Items, Is.EquivalentTo([79uL; 98uL]))
        Assert.That(monkey.IfTrue, Is.EqualTo(2))
        Assert.That(monkey.IfFalse, Is.EqualTo(3))
        Assert.That((monkey.Operation 2uL), Is.EqualTo(38))
        Assert.That(monkey.Test, Is.EqualTo(23uL))

    [<Test>]
    member this.``should parse all monkeys`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> parseAllMonkeys
        
        let numMonkeys = monkeys |> Map.keys |> Seq.length
        Assert.That(numMonkeys, Is.EqualTo(4))
        
    [<Test>]
    member this.``should have correct state after 1st round`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> parseAllMonkeys
            |> performRound divideBy3AndFloor
        
        let expectedOutputs =
            [
                [20; 23; 27; 26]
                [2080; 25; 167; 207; 401; 1046]
                []
                []
            ]
        let monkeysAndExpectedItems =
            expectedOutputs
            |> Seq.mapi (fun i expected -> (Map.find i monkeys), expected)
        
        for monkey, expectedItems in monkeysAndExpectedItems do
            Assert.That(monkey.Items, Is.EquivalentTo(expectedItems))
    
    [<Test>]
    member this.``should have correct num inspections after 20 rounds`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> parseAllMonkeys
            |> performNRounds 20 divideBy3AndFloor
        
        let expectedCounts = [101; 95; 7; 105]
        let monkeysAndExpectedItems =
            expectedCounts
            |> Seq.mapi (fun i expected -> (Map.find i monkeys), expected)
        
        for monkey, expectedCount in monkeysAndExpectedItems do
            Assert.That(monkey.NumInspected, Is.EqualTo(expectedCount))
            
    [<Test>]
    member this.``should calculate correct monkey business score`` () =
        let monkeyBusiness =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> parseAllMonkeys
            |> performNRounds 20 divideBy3AndFloor
            |> calcMonkeyBusiness
        Assert.That(monkeyBusiness, Is.EqualTo(10605))

    [<Test>]
    member this.``should have correct num inspections after 1 round part 2`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> parseAllMonkeys
            |> performRound id
        
        let expectedCounts = [2; 4; 3; 6]
        let actualCounts =
            monkeys
            |> Map.keys
            |> Seq.sort
            |> Seq.map (fun i -> Map.find i monkeys)
            |> Seq.map (fun m -> m.NumInspected)
        
        Assert.That(actualCounts, Is.EquivalentTo(expectedCounts))
        
    [<Test>]
    member this.``should have correct num inspections after 2 rounds part 2`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> parseAllMonkeys
            |> performNRounds 2 id
        
        let expectedCounts = [6; 10; 3; 10]
        let actualCounts =
            monkeys
            |> Map.keys
            |> Seq.sort
            |> Seq.map (fun i -> Map.find i monkeys)
            |> Seq.map (fun m -> m.NumInspected)
        
        Assert.That(actualCounts, Is.EquivalentTo(expectedCounts))
    
    [<Test>]
    member this.``should have correct num inspections after 20 rounds part 2`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> performRoundsModReduction 20
        
        let expectedCounts = [99; 97; 8; 103]
        let actualCounts =
            monkeys
            |> Map.keys
            |> Seq.sort
            |> Seq.map (fun i -> Map.find i monkeys)
            |> Seq.map (fun m -> m.NumInspected)
        
        Assert.That(actualCounts, Is.EquivalentTo(expectedCounts))
            
    [<Test>]
    member this.``should have correct num inspections after 1000 rounds part 2`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> performRoundsModReduction 1000
        
        let expectedCounts = [5204; 4792; 199; 5192]
        let actualCounts =
            monkeys
            |> Map.keys
            |> Seq.sort
            |> Seq.map (fun i -> Map.find i monkeys)
            |> Seq.map (fun m -> m.NumInspected)
        
        Assert.That(actualCounts, Is.EquivalentTo(expectedCounts))
            
    [<Test>]
    member this.``should have correct num inspections after 10,000 rounds part 2`` () =
        let monkeys =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> performRoundsModReduction 10_000
        
        let expectedCounts = [52166; 47830; 1938; 52013]
        let monkeysAndExpectedItems =
            expectedCounts
            |> Seq.mapi (fun i expected -> (Map.find i monkeys), expected)
        
        for monkey, expectedCount in monkeysAndExpectedItems do
            Assert.That(monkey.NumInspected, Is.EqualTo(expectedCount))
    
    [<Test>]
    member this.``should calculate correct monkey business score part 2`` () =
        let monkeyBusiness =
            System.IO.File.ReadLines "Day11/test-input.txt"
            |> performRoundsModReduction 10_000
            |> calcMonkeyBusiness
        Assert.That(monkeyBusiness, Is.EqualTo(2713310158uL))
        
