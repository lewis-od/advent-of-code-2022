module AoC22.UnitTests.Day4.Day4Tests

open NUnit.Framework
open AoC22.Day4.Solution

[<TestFixture>]
type Day4Tests() =

    static member assignmentsAndOverlap =
        [ (((1, 2), (2, 5)), true)
          (((1, 2), (3, 5)), false)
          (((1, 4), (2, 5)), true)
          (((6, 6), (4, 6)), true)
          (((4, 6), (1, 2)), false) ]
        |> Seq.map TestCaseData

    [<Test>]
    member this.``should identify when first assignment contains second``() =
        let first = (2, 10)
        let second = (4, 8)

        let doAssignmentOverlap =
            assignmentContainsOther (first, second)

        Assert.IsTrue(doAssignmentOverlap)

    [<Test>]
    member this.``should identify when second assignment contains first``() =
        let first = (4, 6)
        let second = (3, 8)

        let doAssignmentOverlap =
            assignmentContainsOther (first, second)

        Assert.IsTrue(doAssignmentOverlap)

    [<Test>]
    member this.``should say assignment contains other when they're equal``() =
        let first = (5, 8)

        let doAssignmentsOverlap =
            assignmentContainsOther (first, first)

        Assert.IsTrue(doAssignmentsOverlap)

    [<Test>]
    member this.``should identify when assignment doesn't fully contain another``() =
        let first = (3, 6)
        let second = (7, 9)

        let doAssignmentsOverlap =
            assignmentContainsOther (first, second)

        Assert.IsFalse(doAssignmentsOverlap)

    [<Test>]
    member this.``should count number of fully contained assignments in list``() =
        let numOverlapping =
            System.IO.File.ReadLines "Day4/test-input.txt"
            |> (findNumAssignmentsMatching assignmentContainsOther)

        Assert.AreEqual(2, numOverlapping)

    [<TestCaseSource("assignmentsAndOverlap")>]
    member this.``should identify when 2 assignments overlap at all``(assignments, expectedResult) =
        let actualResult =
            assignmentsOverlap assignments

        Assert.AreEqual(expectedResult, actualResult)

    [<Test>]
    member this.``should count number of overlapping assignments in list`` () =
        let numOverlapping =
            System.IO.File.ReadLines "Day4/test-input.txt"
            |> (findNumAssignmentsMatching assignmentsOverlap)
        Assert.AreEqual(4, numOverlapping)
