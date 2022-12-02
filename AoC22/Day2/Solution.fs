module AoC22.Day2.Solution

type Shape = Rock | Paper | Scissors

type Game = Shape * Shape

let parseTheirLetter letter =
    match letter with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith $"Unexpected their letter: {letter}"
        
let parseMyLetter letter =
    match letter with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith $"Unexpected their letter: {letter}"

let private scoreShape shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let private beatenBy shape =
    match shape with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock
    
let private scoreOutcome ((theirs, mine): Game) =
    let winningMove = beatenBy theirs
    if mine = winningMove then 6
    elif mine = theirs then 3
    else 0

let scoreGame ((theirs, mine): Game) =
    let shapeScore = scoreShape mine
    let outcomeScore = scoreOutcome (theirs, mine)
    shapeScore + outcomeScore
    
let parseShapeRow (row: string): Game =
    let parts = row.Split(' ')
    let theirs = parts[0]
    let mine = parts[1]
    (parseTheirLetter theirs, parseMyLetter mine)
    
let calculateMyScore (input: string seq) =
    input
    |> Seq.map parseShapeRow
    |> Seq.map scoreGame
    |> Seq.sum
    
let part1 () =
    let myScore =
        System.IO.File.ReadLines "Day2/input.txt"
        |> calculateMyScore
    printfn $"My score: {myScore}"

type Outcome = Win | Lose | Draw

let parseOutcome letter =
    match letter with
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith $"Unknown outcome {letter}"

let private beats (shape: Shape) =
    match shape with
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper
    
let findMyShape theirShape outcome =
    match outcome with
    | Win -> beatenBy theirShape
    | Draw -> theirShape
    | Lose -> beats theirShape

let private parseOutcomeRow (row: string) =
    let parts = row.Split(' ')
    let theirs = parseTheirLetter parts[0]
    let outcome = parseOutcome parts[1]
    (theirs, outcome)

let part2 () =
    let myScore =
        System.IO.File.ReadLines "Day2/input.txt"
        |> Seq.map parseOutcomeRow
        |> Seq.map (fun (theirs, outcome) -> (theirs, findMyShape theirs outcome))
        |> Seq.map scoreGame
        |> Seq.sum
    printfn $"My Score {myScore}"
        
