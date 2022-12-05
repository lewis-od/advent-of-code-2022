module AoC22.UnitTests.Day5.Day5Tests

open NUnit.Framework
open AoC22.Day5.Solution

[<TestFixture>]
type Day5Tests () =
    
    [<Test>]
    member this.``should parse initial arrangement`` () =        
        let crateArrangement =
             System.IO.File.ReadAllText "Day5/test-input.txt"
             |> fun input -> input.Split "\n\n"
             |> Array.item 0
             |> parseCrateStacks
        let expectedArrangement =
            Map [
                (1, ['N'; 'Z']);
                (2, ['D'; 'C'; 'M']);
                (3, ['P']);
            ]
        Assert.That(crateArrangement, Is.EquivalentTo(expectedArrangement))
    
    [<Test>]
    member this.``should parse instruction from text`` () =
        let text = "move 3 from 1 to 3"
        let actualInstruction = parseInstruction text
        let expectedInstruction = newInstruction 3 1 3
        Assert.AreEqual(expectedInstruction, actualInstruction)
        
    [<Test>]
    member this.``should apply instruction to stack`` () =
        let instruction = newInstruction 2 1 3
        let initialStacks =
            Map [
                (1, ['N'; 'Z']);
                (2, ['D'; 'C'; 'M']);
                (3, ['P']);
            ]
        let expectedStacks =
            Map [
                (1, []);
                (2, ['D'; 'C'; 'M']);
                (3, ['Z'; 'N'; 'P']);
            ]
        let result = applyInstruction initialStacks instruction
        Assert.That(result, Is.EquivalentTo(expectedStacks))
        
    [<Test>]
    member this.``should find top crates in each stack`` () =
        let message =
            System.IO.File.ReadAllText "Day5/test-input.txt"
            |> findTopCrates applyInstruction
        Assert.AreEqual("CMZ", message)
        
    [<Test>]
    member this.``should find top crates in each stack with new crane model`` () =
        let message =
            System.IO.File.ReadAllText "Day5/test-input.txt"
            |> findTopCrates applyNewInstruction
        Assert.AreEqual("MCD", message)
        