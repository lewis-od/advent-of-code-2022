module AoC22.UnitTests.Day9.Day9Tests

open NUnit.Framework
open AoC22.Day9.Solution

[<TestFixture>]
type Day9Tests () =
    
    static member motions =
        [
            ("R 4", Motion.motion Motion.Right 4)
            ("L 15", Motion.motion Motion.Left 15)
            ("U 2", Motion.motion Motion.Up 2)
            ("D 9", Motion.motion Motion.Down 9)
        ] |> List.map TestCaseData
    
    [<TestCaseSource("motions")>]
    member this.``should parse motion`` (input, expectedCommand) =
        let command = Motion.parse input
        Assert.AreEqual(expectedCommand, command)
    
    [<Test>]
    member this.``should move head left`` () =
        let rope = ((3, 3), (3, 3))
        let motion = Motion.motion Motion.Left 2
        let afterMovement = (rope, motion) ||> Motion.apply
        let expectedRope = ((1, 3), (2, 3))
        Assert.AreEqual(expectedRope, afterMovement)
    
    [<Test>]
    member this.``should move head right`` () =
        let rope = ((3, 3), (3, 3))
        let motion = Motion.motion Motion.Right 3
        let afterMovement = (rope, motion) ||> Motion.apply
        let expectedRope = ((6, 3), (5, 3))
        Assert.AreEqual(expectedRope, afterMovement)
    
    [<Test>]
    member this.``should move head up`` () =
        let rope = ((3, 3), (3, 3))
        let motion = Motion.motion Motion.Up 5
        let afterMovement = (rope, motion) ||> Motion.apply
        let expectedRope = ((3, 8), (3, 7))
        Assert.AreEqual(expectedRope, afterMovement)
        
    [<Test>]
    member this.``should move head down`` () =
        let rope = ((3, 3), (3, 3))
        let motion = Motion.motion Motion.Down 1
        let afterMovement = (rope, motion) ||> Motion.apply
        let expectedRope = ((3, 2), (3, 3))
        Assert.AreEqual(expectedRope, afterMovement)
        
    [<Test>]
    member this.``should move tail diagonally up when head not in same row or column`` () =
        let rope = ((4, 3), (3, 3))
        let motion = Motion.motion Motion.Up 5
        let afterMovement = (rope, motion) ||> Motion.apply
        let expectedRope = ((4, 8), (4, 7))
        Assert.AreEqual(expectedRope, afterMovement)
        
    [<Test>]
    member this.``should move tail diagonally down when head not in same row or column`` () =
        let rope = ((5, 5), (6, 5))
        let motion = Motion.motion Motion.Down 5
        let afterMovement = (rope, motion) ||> Motion.apply
        let expectedRope = ((5, 0), (5, 1))
        Assert.AreEqual(expectedRope, afterMovement)
        
    [<Test>]
    member this.``should convert motion to repeated directions`` () =
        let motion = [Motion.motion Motion.Down 2; Motion.motion Motion.Up 1]
        let directions = motion |> Motion.toDirections
        Assert.That(directions, Is.EquivalentTo([Motion.Down; Motion.Down; Motion.Up]))
    
    [<Test>]
    member this.``doesnt move diagonally when less than 2 away`` () =
        let rope = (1, 4), (2, 4)
        let result = rope |> Motion.moveInDirection Motion.Down
        Assert.AreEqual(((1, 3), (2, 4)), result)
       
    [<Test>]
    member this.``should count distinct tail positions`` () =
       let numPositions =
           System.IO.File.ReadLines "Day9/test-input.txt"
           |> parseMotionsAndCountPositions
       Assert.AreEqual(13, numPositions)