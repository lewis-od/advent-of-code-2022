module AoC22.Day6.Solution

let private pairWith item elements =
    elements
    |> List.map (fun elem -> (item, elem))

let combinations items =
    let rec createCombination lst acc =
        match lst with
        | [] -> acc
        | head::tail -> createCombination tail (acc @ (pairWith head tail))
    createCombination items []

let private charsAreAllDifferent (chars: char list) =
    chars
    |> combinations
    |> Seq.map (fun (a, b) -> a <> b)
    |> Seq.reduce (&&)

let findNumCharsBeforeMarker (packetSize: int) (stream: string) =
    stream.ToCharArray()
    |> Array.toList
    |> List.windowed packetSize
    |> Seq.map charsAreAllDifferent
    |> Seq.findIndex ((=) true)
    |> (+) packetSize
    
let part1 () =
    let numCharacters =
        System.IO.File.ReadAllText "Day6/input.txt"
        |> findNumCharsBeforeMarker 4
    printfn $"Num characters before marker: {numCharacters}"
    
let part2 () =
    let numCharacters =
        System.IO.File.ReadAllText "Day6/input.txt"
        |> findNumCharsBeforeMarker 14
    printfn $"Num characters before marker: {numCharacters}"
