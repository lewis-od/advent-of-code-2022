module AoC22.Day5.Solution

type CrateStack = char list

type CrateStacks = Map<int, CrateStack>

type MoveInstruction = {Quantity: int; FromStack: int; ToStack: int}

let newInstruction quantity fromStack toStack =
    {Quantity = quantity; FromStack = fromStack; ToStack = toStack}

let rec private transpose M = 
    match M with 
    | []::_ -> []
    | _ -> List.map List.head M::transpose(List.map List.tail M)

let parseInstruction (instruction: string) =
    let parts =
        instruction.Split ' '
        |> Array.toList
    
    let quantity, fromStack, toStack =
        match parts with
        | _::quantity::_::fromStack::_::toStack::_ -> (quantity, fromStack, toStack)
        |_ -> failwith "Unexpected instruction format"
    
    newInstruction (int quantity) (int fromStack) (int toStack)

let parseInstructions (instructions: string) =
    instructions.Split "\n"
    |> Array.toList
    |> List.map parseInstruction

let parseCrateStacks (arrangement: string): CrateStacks =
    let rec parseRow (crates: char list) (row: char list): (char list) =
        match row with
        | _::crateLabel::_::' '::rest -> parseRow (crateLabel :: crates) rest
        | _::crateLabel::_ -> (crateLabel :: crates)
        | _ -> failwith "Error"
    
    let parsedRows =
        arrangement.Split "\n"
        |> Array.map Seq.toList
        |> Array.toList
        |> List.map (parseRow [])
    
    let stackNames =
        parsedRows
        |> List.rev
        |> List.head
        |> List.map (string >> int)
        
    let stacks =
        parsedRows
        |> fun rows -> List.take ((List.length rows) - 1) rows // skip last row
        |> transpose
        |> List.map (List.filter ((<>) ' '))

    (Map [], List.zip stackNames stacks)
    ||> List.fold (fun stacks (name, stack) -> Map.add name stack stacks)

let private removeFirstCrate (stack: CrateStack option) =
    match stack with
    | Some value -> Some (List.tail value)
    | None -> None
    
let private addCrate (crate: char) (stack: CrateStack option) =
    match stack with
    | Some value -> Some (crate :: value)
    | None -> None

let moveOneCrate (fromStack: int) (toStack: int) (stacks: CrateStacks) =
    let crateToMove = stacks |> Map.find fromStack |> List.head
    stacks
    |> Map.change fromStack removeFirstCrate
    |> Map.change toStack (addCrate crateToMove)

let applyInstruction (crates: CrateStacks) (instruction: MoveInstruction) =
    let { Quantity = quantity; FromStack = moveFrom; ToStack = moveTo  } = instruction
    
    let moveCrates =
        Seq.init quantity (fun _ -> moveOneCrate moveFrom moveTo)
        |> Seq.reduce (>>)
        
    moveCrates crates

let parseInput (input: string) =
    let parts = input.Split "\n\n"
    let initialState = parseCrateStacks parts[0]
    let instructions = parseInstructions parts[1]
    (initialState, instructions)

let findTopCrates instructionApplier (input: string) =
    let finalState =
        input
        |> parseInput
        ||> List.fold instructionApplier
    Map.keys finalState
    |> Seq.sort
    |> Seq.map (fun key -> Map.find key finalState)
    |> Seq.map Seq.head
    |> Seq.toArray
    |> System.String

let part1 () =
    let message =
        System.IO.File.ReadAllText "Day5/input.txt"
        |> findTopCrates applyInstruction
    printfn $"Message is: {message}"

let private removeFirstCrates (quantity: int) (stack: CrateStack option) =
    match stack with
    | Some value -> Some (List.skip quantity value)
    | None -> None

let private addCrates (crates: char list) (stack: CrateStack option) =
    match stack with
    | Some value -> Some (crates @ value)
    | None -> None

let applyNewInstruction (stacks: CrateStacks) (instruction: MoveInstruction) =
    let { Quantity = quantity; FromStack = fromStack; ToStack = toStack  } = instruction
    
    let cratesToMove = stacks |> Map.find fromStack |> (List.take quantity)
    stacks
    |> Map.change fromStack (removeFirstCrates quantity)
    |> Map.change toStack (addCrates cratesToMove)

let part2 () =
    let message =
        System.IO.File.ReadAllText "Day5/input.txt"
        |> findTopCrates applyNewInstruction
    printfn $"Message is: {message}"
