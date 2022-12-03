module AoC22.UnitTests.Day3.Day3Tests

open NUnit.Framework
open AoC22.Day3.Solution

[<TestFixture>]
type Day3Tests () =

    static member itemsAndBitmaps =
        [
            TestCaseData(['a'; 'b'], 0b110);
            TestCaseData(['a'; 'c'], 0b1010)
        ]

    [<TestCase('a', 1)>]
    [<TestCase('b', 2)>]
    [<TestCase('z', 26)>]
    member this.``should find priority of lower case item`` item expectedPriority =
        let itemPriority = itemPriority item
        Assert.AreEqual(expectedPriority, itemPriority)

    [<TestCase('A', 27)>]
    [<TestCase('B', 28)>]
    [<TestCase('Z', 52)>]
    member this.``should find priority of upper case item`` item expectedPriority =
        let itemPriority = itemPriority item
        Assert.AreEqual(expectedPriority, itemPriority)

    [<Test>]    
    member this.``should set nth bit to 1 where n is the priority`` () =
        let mask = priorityMask 5
        Assert.AreEqual(32, mask)

    [<TestCaseSource("itemsAndBitmaps")>]
    member this.``should compute bitmask for compartment of bag`` (items, expectedMask) =
        let mask = convertItemsToBitmask items
        Assert.AreEqual(expectedMask, mask)

    [<Test>]
    member this.``should find index of non-zero bit`` () =
        let index = indexOfLastNonZeroBit (uint64 0b100)
        Assert.AreEqual(2, index)

    [<Test>]
    member this.``should find priority of duplicated item`` () =
        let priority = findPriorityOfDuplicatedItem "vJrwpWtwJgWrhcsFMMfFFhFp"
        Assert.AreEqual(16, priority)

    [<Test>]
    member this.``should sum priorities of duplicated items`` () =
        let sum = sumPrioritiesOfDuplicates (System.IO.File.ReadLines "Day3/test-input.txt")
        Assert.AreEqual(157, sum)

    [<Test>] 
    member this.``should find priority of badge for group of 3 elves`` () =
        let items = ["vJrwpWtwJgWrhcsFMMfFFhFp"; "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"; "PmmdzqPrVvPwwTWBwg"]
        let priority = findBadgePriorityForGroup items
        Assert.AreEqual(18, priority)
        
    [<Test>]
    member this.``should sum badge priorities of multiple groups`` () =
        let prioritySum = sumPrioritiesOfBadges (System.IO.File.ReadLines "Day3/test-input.txt")
        Assert.AreEqual(70, prioritySum)
