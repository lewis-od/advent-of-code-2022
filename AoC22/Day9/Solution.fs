module AoC22.Day9.Solution

type Position = int * int

type Rope = Position * Position

module Motion =
    type Direction =
        | Left
        | Right
        | Up
        | Down
    
    type T = {Direction: Direction; Distance: int}
    
    let motion (direction: Direction) (distance: int) =
        {Direction=direction; Distance=distance}

    let parse (input: string): T =
        let parts = input.Split(" ")
        let direction, distance = parts[0], (int parts[1])
        let direction =
            match direction with
            | "R" -> Right
            | "L" -> Left
            | "U" -> Up
            | "D" -> Down
            | _ -> failwith $"Unknown direction ${direction}"
        motion direction distance
    
    let movePositionOneStep (direction: Direction) ((x, y): Position): Position =
        match direction with
            | Left -> (x - 1, y)
            | Right -> (x + 1, y)
            | Up -> (x, y + 1)
            | Down -> (x, y - 1)
    
    let private moveHeadOneStep ((head, tail): Rope) (direction: Direction): Rope =
        (movePositionOneStep direction head, tail)
    
    let private moveTailAdjacent ((head, tail): Rope): Rope =
        let dx = (fst head) - (fst tail)
        let dy = (snd head) - (snd tail)
        let tailXCorrection =
            if (abs dx) > 1 then (sign dx)
            else 0
        let tailYCorrection =
            if (abs dy) > 1 then (sign dy)
            else 0
        let x, y = tail
        (head, (x + tailXCorrection, y + tailYCorrection))
        
    let private moveTailDiagonally ((head, (x, y)): Rope) =
        let dx = sign ((fst head) - x)
        let dy = sign ((snd head) - y)
        (head, (x + dx, y + dy))
    
    let private headTailDistance ((head, tail): Rope) =
        let dx = (fst head) - (fst tail)
        let dy = (snd head) - (snd tail)
        (abs dx) + (abs dy)
    
    let updateTail (rope: Rope) =
        let (headX, headY), (tailX, tailY) = rope
        if headX <> tailX && headY <> tailY && (headTailDistance rope) > 2 then moveTailDiagonally rope
        else moveTailAdjacent rope
    
    let moveInDirection (direction: Direction) (rope: Rope): Rope =
        (rope, direction)
        ||> moveHeadOneStep
        |> updateTail
        
    let private toDirection (motion: T) =
        Seq.init motion.Distance (fun _ -> motion.Direction)
        
    let toDirections (motion: T seq) =
        motion |> Seq.collect toDirection
    
    let apply (rope: Rope) (motion: T) =
        let applyMotion =
            Seq.init motion.Distance (fun _ -> moveInDirection motion.Direction)
            |> Seq.reduce (>>)
        rope |> applyMotion

let countDistinctTailPositions (motions: Motion.Direction seq) =
    let initialRope = ((0, 0), (0, 0))
    ([initialRope], motions)
    ||> Seq.fold (fun ropes direction -> (Motion.moveInDirection direction (Seq.head ropes))::ropes)
    |> Seq.map snd
    |> Set.ofSeq
    |> Seq.length

let parseMotions (input: string seq) =
    input
    |> Seq.map Motion.parse
    |> Motion.toDirections
 
let parseMotionsAndCountPositions (input: string seq) =
    input
    |> parseMotions
    |> countDistinctTailPositions

let part1 () =
    let answer =
        System.IO.File.ReadLines "Day9/input.txt"
        |> parseMotionsAndCountPositions
    printfn $"Answer: {answer}"

let moveLongRopeInDirection (direction: Motion.Direction) (knots: Position list) =
    let updatedHead =
        knots
        |> List.head
        |> Motion.movePositionOneStep direction

    let folder (head: Position) (current: Position) =
        let newTail = Motion.updateTail (head, current) |> snd
        (newTail, newTail)
    
    knots
    |> List.skip 1
    |> List.mapFold folder updatedHead
    |> fst
    |> fun knots -> updatedHead::knots


let parseAndMoveLongRope (input: string seq) =
    let knots = List.init 10 (fun _ -> (0, 0))
    
    let folder ((knots, tailPositions): Position list * Set<Position>) (direction: Motion.Direction) =
        let newKnots = (direction, knots) ||> moveLongRopeInDirection
        let tailPos =
            newKnots
            |> List.rev
            |> List.head
        (newKnots, Set.add tailPos tailPositions)

    input
    |> parseMotions
    |> Seq.fold folder (knots, Set.empty<Position>)
    |> snd
    |> Set.count


let part2 () =
    let answer =
        System.IO.File.ReadLines "Day9/input.txt"
        |> parseAndMoveLongRope
    printfn $"Answer: {answer}"