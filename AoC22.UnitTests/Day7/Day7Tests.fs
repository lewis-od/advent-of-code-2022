module AoC22.UnitTests.Day7.Day7Tests

open NUnit.Framework
open AoC22.Day7.Solution

[<TestFixture>]
type Day7Tests () =
    [<Test>]
    member this.``should parse change directory command from input`` () =
        let command = parseCommand "cd .."
        Assert.AreEqual(ChangeDirectory "..", command)
        
    [<Test>]
    member this.``should parse list command from input`` () =
        let command = parseCommand "ls"
        Assert.AreEqual(ListContents, command)
        
    [<Test>]
    member this.``should recursively calculate size of directory`` () =
        let h = file "h.lst" 62596
        let g = file "g" 2557
        let f = file "f" 29116
        let i = file "i" 584
        let e = directory "e" [i]
        let a = directory "a" [e; f; g; h]
        let size = nodeSize a
        Assert.AreEqual(94853, size)
        
    [<Test>]
    member this.``should add file to directory`` () =
        let h = file "h.lst" 62596
        let g = file "g" 2557
        let f = file "f" 29116
        let i = file "i" 584
        let e = directory "e" [i]
        let a = directory "a" [e; f; g]
        
        let result = addChildToDir "e" h a
        
        let expectedE = directory "e" [h; i]
        let expectedResult = directory "a" [expectedE; f; g]
        
        Assert.AreEqual(expectedResult, result)
        
    [<Test>]
    member this.``should parse a command row`` () =
        let result = parseRow "$ cd foo"
        let expected = CommandRow("cd foo")
        Assert.AreEqual(expected, result)
    
    [<Test>]
    member this.``should parse directory node`` () =
        let dir = parseNode "dir" "foo" ["bar"; "/"]
        Assert.AreEqual(directory "//bar/foo" [], dir)
    
    [<Test>]
    member this.``should parse node tree from input`` () =
        let h = file "h.lst" 62596
        let g = file "g" 2557
        let f = file "f" 29116
        let i = file "i" 584
        let e = directory "//a/e" [i]
        let a = directory "//a" [h; g; f; e]
        let b = file "b.txt" 14848514
        let c = file "c.dat" 8504156
        let j = file "j" 4060174
        let dLog = file "d.log" 8033020
        let dExt = file "d.ext" 5626152
        let k = file "k" 7214296
        let d = directory "//d" [k; dExt; dLog; j]
        let root = directory "/" [d; c; b; a]
        
        let input = System.IO.File.ReadLines "Day7/test-input.txt"
        let parsedTree = buildTree input
        
        Assert.AreEqual(root, parsedTree)
        
    [<Test>]
    member this.``should return sizes of all directories`` () =
        let h = file "h.lst" 62596
        let g = file "g" 2557
        let f = file "f" 29116
        let i = file "i" 584
        let e = directory "e" [i]
        let a = directory "a" [h; g; f; e]
        let b = file "b.txt" 14848514
        let c = file "c.dat" 8504156
        let j = file "j" 4060174
        let dLog = file "d.log" 8033020
        let dExt = file "d.ext" 5626152
        let k = file "k" 7214296
        let d = directory "d" [k; dExt; dLog; j]
        let root = directory "/" [d; c; b; a]
        
        let sizes = directorySizes [] root
        
        Assert.AreEqual([584; 94853; 24933642; 48381165], sizes)
        
    [<Test>]
    member this.``should find sum of directories under 100000`` () =
        let sum = System.IO.File.ReadLines "Day7/test-input.txt" |> findDirectorySizesUnderLimit
        
        Assert.AreEqual(95437, sum)
        
    [<Test>]
    member this.``should find the size of the smallest directory to delete`` () =
        let h = file "h.lst" 62596
        let g = file "g" 2557
        let f = file "f" 29116
        let i = file "i" 584
        let e = directory "e" [i]
        let a = directory "a" [h; g; f; e]
        let b = file "b.txt" 14848514
        let c = file "c.dat" 8504156
        let j = file "j" 4060174
        let dLog = file "d.log" 8033020
        let dExt = file "d.ext" 5626152
        let k = file "k" 7214296
        let d = directory "d" [k; dExt; dLog; j]
        let root = directory "/" [d; c; b; a]
        
        let dirSize = findSizeOfDirectoryToDelete root
        
        Assert.AreEqual(24933642, dirSize)
    