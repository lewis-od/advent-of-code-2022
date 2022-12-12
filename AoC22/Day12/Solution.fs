module AoC22.Day12.Solution

open System.Collections.Immutable

module Queue =
    let isEmpty (q: ImmutableQueue<'a>) = q.IsEmpty
    let enqueue elem (q: ImmutableQueue<'a>) = q.Enqueue(elem)
    let dequeue (q: ImmutableQueue<'a>) = q.Dequeue()
    let peek (q: ImmutableQueue<'a>) = q.Peek()
    let empty<'a> = ImmutableQueue<'a>.Empty
    
module Array =
    let neighbours (index1, index2) (array: 'a array array) = seq {
        let deltas = [(0, 1); (0, -1); (1, 0); (-1, 0)]
        for di1, di2 in deltas do
            let newI1 = index1 + di1
            let newI2 = index2 + di2
            if newI1 >= 0 && newI1 < Array.length array &&
               newI2 >= 0 && newI2 < Array.length array[newI1] then yield (newI1, newI2)
    }

type Coords = int * int

let private getHeight (letter: char) =
    match letter with
    | 'S' -> 0
    | 'E' -> 25
    | x -> (int x) - (int 'a')

let toGrid (rows: string seq) =
    rows
    |> Seq.toArray
    |> Array.map (fun row -> row.ToCharArray())
    
let findCoords (letter: char) (grid: char array array) =
    let rowNum =
        grid
        |> Array.findIndex (fun row -> row |> Array.contains letter)
    let colNum =
        grid
        |> Array.item rowNum
        |> Array.findIndex (fun height -> height = letter)
    (rowNum, colNum)

let parseGrid (rows: string seq) =
    let grid = rows |> toGrid
    let start = grid |> findCoords 'S'
    let target = grid |> findCoords 'E'

    let heightMap =
        grid
        |> Array.map (Array.map getHeight)
    
    (heightMap, start, target)

let bfs start target (grid: int array array) =
    let folder point dist (newQ, newSeen) npos =
        if not <| (Set.contains npos newSeen) && (grid[fst npos][snd npos] <= grid[fst point][snd point] + 1) then
            (Queue.enqueue (dist + 1, npos) newQ, Set.add npos newSeen)
        else
            (newQ, newSeen)
    
    let rec doSearch toVisit seen =
        if Queue.isEmpty toVisit then None
        else
            let dist, point = Queue.peek toVisit
            if point = target then Some(dist)
            else
                let newToVisit, newSeen =
                    Array.neighbours point grid
                    |> Seq.fold (folder point dist) (Queue.dequeue toVisit, seen)
                doSearch newToVisit newSeen

    let q = Queue.enqueue (0, start) Queue.empty
    let seen = Set.add start Set.empty
    doSearch q seen
            

let findShortestPathPath (rows: string seq) =
    let grid, start, target = rows |> parseGrid
    let result = bfs start target grid
    match result with
    | Some distance -> distance
    | None -> failwith "Unable to find path"

let part1 () =
    let answer =
        System.IO.File.ReadLines "Day12/input.txt"
        |> findShortestPathPath
    printfn $"Shortest path is {answer} steps long"

let findStartingPoints (grid: int array array) =
    let findCoordsInRow (row: int array) =
        row
        |> Array.indexed
        |> Array.filter (fun (_, height) -> height = 0)
        |> Array.map fst
        
    grid
    |> Array.indexed
    |> Array.collect (fun (x, row) -> findCoordsInRow row |> Array.map (fun y -> (x, y)))

let findShortestPathFromGround (rows: string seq) =
    let grid, _, target = rows |> parseGrid
    grid
    |> findStartingPoints
    |> Array.map (fun start -> bfs start target grid)
    |> Array.filter (fun answer -> answer.IsSome)
    |> Array.map (fun x -> x.Value)
    |> Array.min

let part2 () =
    let answer =
        System.IO.File.ReadLines "Day12/input.txt"
        |> findShortestPathFromGround
    printfn $"Shortest path is {answer} steps long"
