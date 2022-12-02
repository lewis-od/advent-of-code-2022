module AoC22.UnitTests.Day2.Day2Tests

open NUnit.Framework
open AoC22.Day2.Solution

[<TestFixture>]
type Day2Tests () =
    
    [<Test>]
    member this.``should map their A to rock`` () =
        let theirShape = parseTheirLetter "A"
        Assert.AreEqual(Rock, theirShape)
        
    [<Test>]
    member this.``should map their B to paper`` () =
        let theirShape = parseTheirLetter "B"
        Assert.AreEqual(Paper, theirShape)
        
    [<Test>]
    member this.``should map their C to scissors`` () =
        let theirShape = parseTheirLetter "C"
        Assert.AreEqual(Scissors, theirShape)
        
    [<Test>]
    member this.``should map my X to rock`` () =
        let theirShape = parseMyLetter "X"
        Assert.AreEqual(Rock, theirShape)
        
    [<Test>]
    member this.``should map my Y to paper`` () =
        let theirShape = parseMyLetter "Y"
        Assert.AreEqual(Paper, theirShape)
        
    [<Test>]
    member this.``should map my Z to scissors`` () =
        let theirShape = parseMyLetter "Z"
        Assert.AreEqual(Scissors, theirShape)
        
    [<Test>]
    member this.``should score a winning game`` () =
        let score = scoreGame (Rock, Paper)
        Assert.AreEqual(8, score)
        
    [<Test>]
    member this.``should score a losing game`` () =
        let score = scoreGame (Paper, Rock)
        Assert.AreEqual(1, score)
        
    [<Test>]
    member this.``should score a drawing game`` () =
        let score = scoreGame (Scissors, Scissors)
        Assert.AreEqual(6, score)
        
    [<Test>]
    member this.``should calculate my score`` () =
        let input = System.IO.File.ReadLines "Day2/test-input.txt"
        let myScore = calculateMyScore input
        Assert.AreEqual(15, myScore)