module AoC22.UnitTests.Day8.Day8Tests

open NUnit.Framework
open AoC22.Day8.Solution

[<TestFixture>]
type Day8Tests () =
    
    let grid =
        [
            [tree 3; tree 0; tree 3; tree 7; tree 3]
            [tree 2; tree 5; tree 5; tree 1; tree 2]
            [tree 6; tree 5; tree 3; tree 3; tree 2]
            [tree 3; tree 3; tree 5; tree 4; tree 9]
            [tree 3; tree 5; tree 3; tree 9; tree 0]
        ]
    
    [<Test>]
    member this.``should parse input to 2d array`` () =
        let parsedGrid =
            System.IO.File.ReadLines "Day8/test-input.txt"
            |> parseGrid
        Assert.That(parsedGrid, Is.EquivalentTo(grid))
        
    [<Test>]
    member this.``should mark number of visible trees in a row from left to right`` () =
        let row = [tree 2; tree 5; tree 5; tree 1; tree 2]
        let numVisible = markVisibleTreesLeftToRight row
        let expected = [markVisible (tree 2); markVisible (tree 5); tree 5; tree 1; tree 2]
        Assert.That(numVisible, Is.EquivalentTo(expected))
        
    [<Test>]
    member this.``should mark number of visible trees in both horizontal directions`` () =
        let row = [tree 2; tree 5; tree 5; tree 1; tree 2]
        let numVisible = markVisibleTreesHorizontally row
        let expected = [markVisible (tree 2); markVisible (tree 5); markVisible (tree 5); tree 1; markVisible (tree 2)]
        Assert.That(numVisible, Is.EquivalentTo(expected))
        
    [<Test>]
    member this.``should count all visible trees in grid`` () =
        let numVisible = grid |> countVisibleTreesInGrid
        Assert.AreEqual(21, numVisible)
        
    [<Test>]
    member this.``should count trees smaller than current from R to L stopping when one is the same height`` () =
        let heights = [3; 1; 5; 7; 2]
        let numSmaller = smallerRtoL 5 heights
        Assert.AreEqual(3, numSmaller)
        
    [<Test>]
    member this.``should count trees smaller than current from R to L stopping at the edge of the grid`` () =
        let heights = [3; 1; 4; 2; 4]
        let numSmaller = smallerRtoL 5 heights
        Assert.AreEqual(5, numSmaller)
        
    [<Test>]
    member this.``should calculate the score horizontally of a given tree`` () =
        let heights =
            grid
            |> List.map (List.map (fun t -> t.Height))
        let hScore = horizontalScore heights (1, 2)
        Assert.AreEqual(2, hScore)
        
    [<Test>]
    member this.``should calculate the score horizontally of another tree`` () =
        let heights =
            grid
            |> List.map (List.map (fun t -> t.Height))
        let hScore = horizontalScore heights (3, 2)
        Assert.AreEqual(4, hScore)
        
    [<Test>]
    member this.``should generate positions`` () =
        let heights = [ [1; 2; 3]; [4; 5; 6] ]
        let expected = [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (1, 2) ]
        let positions = generatePositions heights
        Assert.That(positions, Is.EquivalentTo(expected))

    [<Test>]
    member this.``should find highest scenic score`` () =
        let score = calcHighestScore grid
        Assert.AreEqual(8, score)