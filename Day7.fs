module aoc_2022.Day7

open FParsec
open System.IO
open Microsoft.FSharp.Collections
open ParsingUtils

type DirEntry =
    | FileSize of int
    | ChildDir of Directory
    
and Directory =
    {
        Entries: Map<string, DirEntry>;
        Size: int
    }

module Directory =
    let empty = {
        Entries = Map.empty;
        Size = 0;
    }
    let assumeDir x =
        match x with
        | ChildDir d -> d
        | _ -> failwith "Value is not a ChildDir"
        
    let rec ensureDirectory name path dir =
        match path with
        | [] ->
            let childDir =
                dir.Entries
                |> Map.tryFind name
                |> Option.map assumeDir
                |> Option.orElse ({ Size = 0; Entries = Map.empty } |> Some)
                |> Option.get
                |> ChildDir
                
            let newEntries =
                dir.Entries
                |> Map.add name childDir
                
            { dir with Entries = newEntries }
        | pathSegment :: remainingPath ->
            let newChildDir =
                dir.Entries[pathSegment]
                |> assumeDir
                |> ensureDirectory name remainingPath
                |> ChildDir
                
            let newEntries =
                dir.Entries
                |> Map.add pathSegment newChildDir
                
            { dir with Entries = newEntries }
            
    let private getSize entry =
        match entry with
        | FileSize s -> s
        | ChildDir d -> d.Size
            
    let rec addFile name path size dir =
        let newEntries =
            match path with
            | [] ->
                dir.Entries
                |> Map.add name (size |> FileSize)
            | segment :: remainingPath ->
                let newChildDir =
                    dir.Entries[segment]
                    |> assumeDir
                    |> addFile name remainingPath size
                    |> ChildDir
                    
                dir.Entries
                |> Map.add segment newChildDir
                
        let newSize =
            newEntries
            |> Map.values
            |> Seq.map getSize
            |> Seq.sum
            
        { Entries = newEntries; Size = newSize }

type ParserState = {
    DirStack: string list;
    Filesystem: Directory;
}

module parsing =
    let handleCd dirName state =
        if dirName = "/"
        then { state with DirStack = [] }
        else if dirName = ".."
        then { state with DirStack = state.DirStack.Tail }
        else { state with DirStack = dirName :: state.DirStack }
        
    let handleAddFile name size state =
        let path = state.DirStack |> List.rev
        {
            state with
                Filesystem =
                    Directory.addFile name path size state.Filesystem
        }
        
    let handleEnsureDirectory name state =
        let path = state.DirStack |> List.rev
        {
            state with
                Filesystem =
                    Directory.ensureDirectory name path state.Filesystem
        }
        
    let parseCd =
        skipString "$ cd "
        >>. restOfLine true
        >>= (fun dirName -> updateUserState (handleCd dirName))
        
    let parseFileListing =
        tuple2 pint32 (skipChar ' ' >>. restOfLine true)
        >>= (fun (size, filename) -> updateUserState (handleAddFile filename size))
        
    let parseSubdirectory =
        skipString "dir "
        >>. restOfLine true
        >>= (fun dirName -> updateUserState (handleEnsureDirectory dirName))
        
    let parseLs =
        skipString "$ ls" .>> restOfLine true
        >>. many (parseFileListing <|> parseSubdirectory)
        >>. preturn ()
        
    let parseCommand =
        choice [
            parseCd;
            parseLs;
        ]
        
    let parseCommands =
        (many parseCommand)
        >>. getUserState
        |>> fun st -> st.Filesystem


let private parseFile = lazy (
    let lines = File.ReadAllLines "./day7.txt"
    
    let fileText = System.String.Join("\n", lines)
    let initialState = { Filesystem = Directory.empty; DirStack = [] }
    
    runParserWithState parsing.parseCommands fileText initialState
)

let toDirOption x =
    match x with
    | ChildDir dir -> Some dir
    | _ -> None
    
let rec dirs root =
    seq {
        let childDirs =
            root.Entries
            |> Map.values
            |> Seq.map toDirOption
            |> Seq.collect Option.toList
            
        yield! childDirs
        
        for dir in childDirs do
            yield! dirs dir
    }

let part1 () =
    let filesystem = parseFile.Force()
    
    let dirSizes =
        dirs filesystem
        |> Seq.map (fun x -> x.Size)
        |> Seq.filter (fun x -> x < 100000)
        |> Seq.sum
        
    dirSizes |> string
    
let part2 () =
    let filesystem = parseFile.Force()
    
    let freeSpace = 70000000 - filesystem.Size
    let spaceNeeded = 30000000
    let toDelete = spaceNeeded - freeSpace
    
    let dirToDelete =
        dirs filesystem
        |> Seq.map (fun x -> x.Size)
        |> Seq.filter (fun x -> x >= toDelete)
        |> Seq.sort
        |> Seq.head
        
    dirToDelete |> string
