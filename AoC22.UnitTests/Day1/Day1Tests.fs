namespace AoC22.UnitTests.Day1.Day1Tests

open AoC22.Day1.Solution
open NUnit.Framework

[<TestFixture>]
type Day1Tests () =

    [<Test>]
    member this.``should find most amount of calories of any elf`` () =
        let input = System.IO.File.ReadAllText "Day1/test-input.txt"
        let calories = calculateAnswer Seq.max input
        Assert.AreEqual(24000, calories)
        
        
    [<Test>]
    member this.``should find sum of 3 elves with most calories``() =
        let input = System.IO.File.ReadAllText "Day1/test-input.txt"
        let calories = calculateAnswer findSumOfTop3 input
        Assert.AreEqual(45000, calories)
