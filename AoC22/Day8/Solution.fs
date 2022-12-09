module AoC22.Day8.Solution

type Tree = {Height: int; IsVisible:bool}

let tree height = {Height=height; IsVisible=false}

let markVisible tree = {Height=tree.Height; IsVisible=true}

let parseGrid (input: string seq): Tree list list =
    input
    |> Seq.map (fun x -> Array.toList (x.ToCharArray()))
    |> Seq.map (List.map (string >> int >> tree))
    |> Seq.toList

let markVisibleTreesLeftToRight (row: Tree list): Tree list =
    let marker (tallest: int, acc: Tree list) (currentTree: Tree) =
        if currentTree.Height > tallest then (currentTree.Height, (markVisible currentTree)::acc)
        else (tallest, currentTree::acc)
    let first = row |> List.head |> markVisible
    let middle = row |> List.skip 1

    ((first.Height, [markVisible first]), middle)
    ||> List.fold marker
    |> snd
    
let markVisibleTreesHorizontally (row: Tree list): Tree list =
    row
    |> markVisibleTreesLeftToRight
    |> markVisibleTreesLeftToRight

let countVisibleTreesInGrid (grid: Tree list list): int =
    grid
    |> List.map markVisibleTreesHorizontally
    |> List.transpose
    |> List.map markVisibleTreesHorizontally
    |> List.transpose
    |> List.map ((List.filter (fun t -> t.IsVisible)) >> List.length)
    |> List.sum

let part1 () =
    let answer =
        System.IO.File.ReadLines "Day8/input.txt"
        |> parseGrid
        |> countVisibleTreesInGrid
    printfn $"Num visible trees: {answer}"

let smallerRtoL (treeHeight: int) (treeHeights: int list): int =
    let rec countSmallerTrees (currentTree: int) (numTaller: int) (remaining: int list) =
        match remaining with
        | next::rest ->
            if next >= currentTree then (numTaller + 1)
            else countSmallerTrees currentTree (numTaller + 1) rest
        | [] -> numTaller
    
    countSmallerTrees treeHeight 0 treeHeights

let horizontalScore (trees: int list list) (position: int * int) =
    let row, col = position
    let rowHeights =
        trees
        |> List.item row
        |> List.skip col
    let lrScore =
        match rowHeights with
        | head::tail -> smallerRtoL head tail
        | _ -> 0
    
    let rlHeights =
        trees
        |> List.item row
        |> List.rev
        |> List.skip ((List.length trees[0]) - col - 1)
    let rlScore =
        match rlHeights with
        | head::tail -> smallerRtoL head tail
        | _ -> 0
    
    lrScore * rlScore

let generatePositions (trees: int list list) =
    let numRows = trees |> List.length
    let numCols = trees |> List.head |> List.length
    [0..(numRows - 1)]
    |> List.collect (fun row -> (List.init numCols (fun col -> (row, col))))

let heights (trees: Tree list list) = trees |> List.map (List.map (fun t -> t.Height))

let updateScore (newScore: int) (existingScore: int option): int option =
    match existingScore with
    | Some value -> Some(value * newScore)
    | None -> None

let calcHighestScore (trees: Tree list list) =
    let treeHeights = trees |> heights
    let positions = treeHeights |> generatePositions
    let horizontalScores =
        positions
        |> List.map (fun coords -> (coords, (horizontalScore treeHeights coords)))
        |> List.fold (fun scores (coords, score) -> Map.add coords score scores) (Map [])

    let verticalScores =
        positions
        |> List.map (fun (x, y) -> (x, y), (horizontalScore (List.transpose treeHeights) (y, x)))
        |> List.fold (fun scores (coords, score) -> Map.change coords (updateScore score) scores) (horizontalScores)

    verticalScores
    |> Map.values
    |> Seq.max
    
let part2 () =
    let score =
        System.IO.File.ReadLines "Day8/input.txt"
        |> parseGrid
        |> calcHighestScore
    printfn $"Score: {score}"
