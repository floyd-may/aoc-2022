module aoc_2022.Day7Tests

open Xunit
open FsUnit.Xunit
open ParsingUtils
open Day7
let sampleInput = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

let sampleInput2 = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
"""

let expectedSampleDir2 = {
    Entries = [
            "a", ChildDir {
                Entries = [
                    "e", ChildDir {
                        Entries = [
                            "i", FileSize 584
                        ]
                        |> Map.ofList
                        Size = 584
                    }
                    "f", FileSize 29116;
                    "g", FileSize 2557;
                    "h.lst", FileSize 62596;
                ]
                |> Map.ofList
                Size = 29116 + 2557 + 62596 + 584
            };
            "b.txt", FileSize 14848514;
            "c.dat", FileSize 8504156;
            "d", ChildDir {
                Entries = Map.empty;
                Size = 0
            };
        ]
        |> Map.ofList
    Size = 14848514 + 8504156 + 29116 + 2557 + 62596 + 584
}

let expectedSampleDir = {
    Entries = [
            "a", ChildDir {
                Entries = [
                    "e", ChildDir {
                        Entries = [
                            "i", FileSize 584
                        ]
                        |> Map.ofList
                        Size = 584
                    }
                    "f", FileSize 29116;
                    "g", FileSize 2557;
                    "h.lst", FileSize 62596;
                ]
                |> Map.ofList
                Size = 94853
            };
            "b.txt", FileSize 14848514;
            "c.dat", FileSize 8504156;
            "d", ChildDir {
                Entries = [
                    "j", FileSize 4060174;
                    "d.log", FileSize 8033020;
                    "d.ext", FileSize 5626152;
                    "k", FileSize 7214296;
                ]
                |> Map.ofList
                Size = 24933642
            };
            
        
        ]
        |> Map.ofList
    Size = 48381165
}

[<Fact>]
let ``parses sample input`` () =
    let initialState = { Filesystem = Directory.empty; DirStack = [] }
    let result = runParserWithState parsing.parseCommands sampleInput initialState
    
    result |> should equal expectedSampleDir
    
[<Fact>]
let ``parses sample input 2`` () =
    let initialState = { Filesystem = Directory.empty; DirStack = [] }
    let result = runParserWithState parsing.parseCommands sampleInput2 initialState
    
    result |> should equal expectedSampleDir2
