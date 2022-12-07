module AoC22.Day7.Solution

type Command =
    | ChangeDirectory of string
    | ListContents

let parseCommand (input: string): Command =
    let parts = input.Split(" ")
    match parts[0] with
    | "cd" -> ChangeDirectory parts[1]
    | "ls" -> ListContents
    | _ -> failwith $"Unknown command {input}"

type FileSystemNode =
    | File of File
    | Directory of Directory
and File = {Name: string; Size: int}
and Directory = {Name: string; Children: FileSystemNode list}

let directory name children =
    Directory {Name=name; Children=children}

let file name size =
    File {Name=name; Size = size}

let rec private cataFileSystem fFile fDirectory item: 'r =
    let recurse = cataFileSystem fFile fDirectory
    match item with
    | File file ->
        fFile file
    | Directory dir ->
        let listOfRs = dir.Children |> List.map recurse
        fDirectory (dir.Name, listOfRs)

let nodeSize fsItem =
    let fFile (file: File) = file.Size
    let fDirectory (_, childSizes: int list) =
        childSizes |> List.sum
    cataFileSystem fFile fDirectory fsItem
    
let addChildToDir (dirName: string) (child: FileSystemNode) (root: FileSystemNode) =
    let fFile (file: File) = File file
    let fDirectory (name: string, children: FileSystemNode list) =
        if name = dirName then directory name (child::children)
        else directory name children
    cataFileSystem fFile fDirectory root

type Row =
    | CommandRow of string
    | NodeRow of string * string

let parseRow (row: string) =
    if row.StartsWith('$') then
        CommandRow(row.Substring 2)
    else
        let parts = row.Split(" ")
        NodeRow(parts[0], parts[1])
 
let stackToPath (cdStack: string list) =
    cdStack
    |> List.rev
    |> String.concat "/" 
      
let parseNode (first: string) (second: string) (cdStack: string list) =
    if first = "dir" then
        directory (stackToPath (second::cdStack)) []
    else file second (int first)

let handleCommand (command: Command) (fs: FileSystemNode, cdStack: string list) =
    match command with
    | ChangeDirectory s ->
        if s = ".." then (fs, List.tail cdStack)
        else (fs, s::cdStack)
    | ListContents -> (fs, cdStack)

let handleRow (fs: FileSystemNode, cdStack: string list) (row: Row) =
    match row with
        | NodeRow(first, second) ->
            let node = parseNode first second cdStack
            let updatedFs = addChildToDir (stackToPath cdStack) node fs
            (updatedFs, cdStack)
        | CommandRow(commandText) ->
            let command = parseCommand commandText
            handleCommand command (fs, cdStack)
            
let buildTree (rows: string seq) =
    rows
    |> Seq.skip 1
    |> Seq.map parseRow
    |> Seq.fold handleRow ((directory "/" []), ["/"])
    |> fst

let rec directorySizes (acc: int list) (item: FileSystemNode): int list =
    match item with
    | File _ -> acc
    | Directory dir ->
        let newAcc = (nodeSize (Directory dir)) :: acc
        dir.Children
        |> List.fold directorySizes newAcc

let sumOfSizesLessThanOrEqualTo maxSize fs =
    ([], fs)
    ||> directorySizes
    |> List.filter (fun x -> x <= maxSize)
    |> List.sum
    
let findDirectorySizesUnderLimit (input: string seq) =
    input
    |> buildTree
    |> sumOfSizesLessThanOrEqualTo 100000
    
let part1 () =
    let result =
        System.IO.File.ReadLines "Day7/input.txt"
        |> findDirectorySizesUnderLimit
    printfn $"Answer: {result}"

let findSizeOfDirectoryToDelete (fs: FileSystemNode) =
    let freeSpace = 70000000 - (nodeSize fs)
    let requiredToFree = 30000000 - freeSpace
    ([], fs)
    ||> directorySizes
    |> List.filter (fun x -> x >= requiredToFree)
    |> List.min

let part2 () =
    let result =
        System.IO.File.ReadLines "Day7/input.txt"
        |> buildTree
        |> findSizeOfDirectoryToDelete
    printfn $"Answer: {result}"
