module AoC22.UnitTests.Day10.Day10Tests

open NUnit.Framework
open AoC22.Day10.Solution

[<TestFixture>]
type Day10Tests () =
    
    static member outputsAndPixels =
        [
            (1, 0), '#'
            (1, 1), '#'
            (1, 2), '.'
            (34, 32), '#'
            (34, 31), '.'
            (40, 38), '#'
            (81, 1), '#'
            (81, 80), '.'
        ] |> Seq.map TestCaseData
    
    [<Test>]
    member this.``should parse addx instruction`` () =
        let instruction = Instruction.parse "addx 4"
        let expected = Instruction.AddX(4)
        Assert.AreEqual(expected, instruction)
        
    [<Test>]
    member this.``should parse noop instruction`` () =
        let instruction = Instruction.parse "noop"
        let expected = Instruction.NoOp
        Assert.AreEqual(expected, instruction)
    
    [<Test>]
    member this.``should process list of instructions`` () =
        let input = ["noop"; "addx 3"; "addx -5"]
        let result = applyInstructions input
        let expectedResult = [(1, 1); (2, 1); (3, 1); (4, 4); (5, 4); (6, -1)]
        Assert.That(result, Is.EquivalentTo(expectedResult))
        
    [<Test>]
    member this.``should sample signal strengths`` () =
        let signalStrengths =
            System.IO.File.ReadLines "Day10/test-input.txt"
            |> applyInstructions
            |> sampleSignalStrength
        let expectedSignalStrengths = [420; 1140; 1800; 2940; 2880; 3960]
        Assert.That(signalStrengths, Is.EquivalentTo(expectedSignalStrengths))
        
    [<TestCaseSource("outputsAndPixels")>]
    member this.``should correctly draw pixel (or not)`` (state, expectedPixel) =
        let actualPixel = drawPixel state
        Assert.AreEqual(expectedPixel, actualPixel)
