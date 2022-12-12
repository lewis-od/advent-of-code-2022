module AoC22.UnitTests.Day12.Day12Tests

open NUnit.Framework
open AoC22.Day12.Solution

[<TestFixture>]
type Day12Tests () =
    [<Test>]
    member this.``should find shortest path from start`` () =
        let distance =
            System.IO.File.ReadLines "Day12/test-input.txt"
            |> findShortestPathPath
        Assert.AreEqual(31, distance)
        
    [<Test>]
    member this.``should find all starting points`` () =
        let grid, _, _ =
            System.IO.File.ReadLines "Day12/test-input.txt"
            |> parseGrid
        
        let startingPoints = findStartingPoints grid
        
        let expectedPoints = [(0, 0); (0, 1); (1, 0); (2, 0); (3, 0); (4, 0)]
        
        Assert.That(startingPoints, Is.EquivalentTo(expectedPoints))
        
    [<Test>]
    member this.``should find shortest path from ground`` () =
        let distance =
            System.IO.File.ReadLines "Day12/test-input.txt"
            |> findShortestPathFromGround
        Assert.AreEqual(29, distance)

