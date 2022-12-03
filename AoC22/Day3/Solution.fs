module AoC22.Day3.Solution

let private alphabet = ['a' .. 'z'] @ ['A' .. 'Z']

let itemPriority character =
    let index = List.findIndex ((=) character) alphabet
    index + 1

let priorityMask (priorityScore: int): uint64 =
    (uint64 1) <<< priorityScore

let convertItemsToBitmask (items: char seq): uint64 =
    items
    |> Seq.map itemPriority
    |> Seq.map priorityMask
    |> Seq.reduce (|||)

let indexOfLastNonZeroBit (mask: uint64) =
    let rec findIndexOfNonZeroBit (mask: uint64) (counter: int) =
        if mask = (uint64 1) then counter
        else findIndexOfNonZeroBit (mask >>> 1) (counter + 1)
    findIndexOfNonZeroBit mask 0

let findPriorityOfDuplicatedItem (itemString: string) =
    let items = Seq.toList itemString
    let compartmentSize = (List.length items) / 2
    
    let firstCompartment = items |> List.take compartmentSize
    let secondCompartment = items |> List.skip compartmentSize
    
    [firstCompartment; secondCompartment]
        |> Seq.map convertItemsToBitmask
        |> Seq.reduce (&&&)
        |> indexOfLastNonZeroBit

let sumPrioritiesOfDuplicates (rucksacks: string seq) =
    rucksacks
    |> Seq.map findPriorityOfDuplicatedItem
    |> Seq.sum

let part1 () =
    let answer = sumPrioritiesOfDuplicates (System.IO.File.ReadLines "Day3/input.txt")
    printfn $"Answer: {answer}"

let findBadgePriorityForGroup (groupItems: string seq) =
    groupItems
    |> Seq.map Seq.toList
    |> Seq.map convertItemsToBitmask
    |> Seq.reduce (&&&)
    |> indexOfLastNonZeroBit
    
let sumPrioritiesOfBadges (allRucksacks: string seq) =
    allRucksacks
    |> Seq.chunkBySize 3
    |> Seq.map findBadgePriorityForGroup
    |> Seq.sum

let part2 () =
    let answer = sumPrioritiesOfBadges (System.IO.File.ReadLines "Day3/input.txt")
    printfn $"Answer: {answer}"
