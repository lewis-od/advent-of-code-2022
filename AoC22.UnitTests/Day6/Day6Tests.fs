module AoC22.UnitTests.Day6.Day6Tests

open NUnit.Framework
open AoC22.Day6.Solution

[<TestFixture>]
type Day6Tests () =
    
    [<Test>]
    member this.``should return all combinations of 2 items in a list`` () =
        let input = ['a'; 'b'; 'c'; 'd']
        let combs = combinations input
        let expectedCombs = [('a', 'b'); ('a', 'c'); ('a', 'd'); ('b', 'c'); ('b', 'd'); ('c', 'd')]
        Assert.That(combs, Is.EquivalentTo(expectedCombs))
    
    [<TestCase("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
    [<TestCase("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
    [<TestCase("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
    [<TestCase("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
    member this.``should count num chars before first 4 different chars`` (input, expectedAnswer) =
        let answer = findNumCharsBeforeMarker 4 input
        Assert.AreEqual(expectedAnswer, answer)
    