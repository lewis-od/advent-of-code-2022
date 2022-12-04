module AoC22.Day4.Solution

type Assigment = int * int

let assignmentContainsOther (elfOne, elfTwo) =
    ((fst elfOne) <= (fst elfTwo) && (snd elfOne) >= (snd elfTwo))
    || (fst elfTwo) <= (fst elfOne) && (snd elfTwo) >= (snd elfOne)

let private parseAssignment (assignment: string) =
    let parts = assignment.Split '-'
    (int parts[0]), (int parts[1])

let private parseRow (row: string): Assigment * Assigment =
    let parts = row.Split ','
    (parseAssignment parts[0]), (parseAssignment parts[1])
    
let private countAssignmentsMatching predicate (assignments: (Assigment * Assigment) seq) =
    assignments
    |> Seq.filter predicate
    |> Seq.length
    
let findNumAssignmentsMatching predicate (rows: string seq) =
    rows
    |> Seq.map parseRow
    |> (countAssignmentsMatching predicate)
    
let part1 () =
    let numContained =
        System.IO.File.ReadLines "Day4/input.txt"
        |> (findNumAssignmentsMatching assignmentContainsOther)
    printfn $"Number of fully contained assignments: {numContained}"
    
let assignmentsOverlap (elfOne, elfTwo) =
    let sorted =
        [elfOne; elfTwo]
        |> List.sortBy fst
    let first = sorted |> List.item 0
    let second = sorted |> List.item 1
    
    (snd first) >= (fst second)

let part2 () =
    let numOverlapping =
        System.IO.File.ReadLines "Day4/input.txt"
        |> (findNumAssignmentsMatching assignmentsOverlap)
    printfn $"Number of overlapping assignments {numOverlapping}"
