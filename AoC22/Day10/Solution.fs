module AoC22.Day10.Solution

type Computer = { X: int; }

module Instruction =
    type T =
        | NoOp
        | AddX of int
        
    let parse (input: string): T =
        let parts = input.Split(" ")
        match parts[0] with
        | "noop" -> NoOp
        | "addx" -> AddX(int parts[1])
        | _ -> failwith $"Unknown instruction '{input}'"

    let toCycles (instruction: T): T seq =
        match instruction with
        | NoOp -> [NoOp]
        | AddX i -> [NoOp; AddX(i)]
    
    let apply (state: Computer) (instruction: T): Computer =
        match instruction with
        | NoOp -> state
        | AddX i -> {X=state.X + i}

let toCycles (instructions: Instruction.T seq) =
    instructions
    |> Seq.collect Instruction.toCycles


let pairWithCycleNum (states: Computer seq): (int * int) seq =
    states
    |> Seq.indexed
    |> Seq.map (fun (index, state) -> (index + 2), state.X)

let applyInstructions (input: string seq): (int * int) seq =
    let folder computer instruction =
        let result = Instruction.apply computer instruction
        (result, result)
    
    input
    |> Seq.map Instruction.parse
    |> toCycles
    |> Seq.mapFold folder {X=1}
    |> fst
    |> pairWithCycleNum
    |> Seq.append (Seq.singleton (1, 1))

let sampleSignalStrength (states: (int * int) seq): int seq =
    let shouldSample cycleNum =
        if cycleNum < 20 then false
        else (cycleNum - 20) % 40 = 0
    
    states
    |> Seq.filter (fst >> shouldSample)
    |> Seq.map (fun (cycle, state) -> cycle * state)

let part1 () =
    let answer =
        System.IO.File.ReadLines "Day10/input.txt"
        |> applyInstructions
        |> sampleSignalStrength
        |> Seq.sum
    printfn $"Answer: {answer}"

let drawPixel ((cycle, x): int * int) =
    let pixelIndex = (cycle - 1) % 40
    if abs (pixelIndex - x) <= 1 then '#'
    else '.'

let part2 () =
    let rows =
        System.IO.File.ReadLines "Day10/input.txt"
        |> applyInstructions
        |> Seq.map drawPixel
        |> Seq.chunkBySize 40
        
    for row in rows do
        for pixel in row do
            printf $"{pixel}"
        printf "\n"
