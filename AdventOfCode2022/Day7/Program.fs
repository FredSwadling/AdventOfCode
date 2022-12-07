// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Text.RegularExpressions

type Line =
    | ChangeDirectory of string
    | GoUpDirectory
    | Lis
    | DirLine of string
    | FileLine of int * string
   
type FileOrDir =
      | Fil of string * int
      | Dir of string * FileOrDir list
      
module Dir =
      let rec private buildDir dirSoFar = function
            | Lis::remaining -> buildDir dirSoFar remaining
            | DirLine dir::remaining -> buildDir ((Dir (dir, []))::dirSoFar) remaining
            | FileLine (size, file)::remaining -> buildDir ((Fil (file, size))::dirSoFar) remaining
            | ChangeDirectory dir::remaining ->
                  match List.tryFind (function | Dir (name, _) when name = dir -> true | _ -> false) dirSoFar with
                  | Some (Dir (n, d)) ->
                        let newDir, remaining = buildDir d remaining
                        let dirSoFar = (Dir (n, newDir))::dirSoFar
                        buildDir dirSoFar remaining
                  | None ->
                        let newDir, remaining = buildDir [] remaining
                        let dirSoFar = (Dir (dir, newDir))::dirSoFar
                        buildDir dirSoFar remaining
                  | Some (Fil _) -> failwith "oh oh spagetti ohs"
            | GoUpDirectory::remaining -> dirSoFar, remaining
            | [] -> dirSoFar, []
            
      let rec getSize = function
            | Fil (_, count) -> count
            | Dir (_, dir) ->
                  dir
                  |> Seq.map getSize
                  |> Seq.sum
                  
      let rec getDirs = function
            | Fil _ -> Seq.empty
            | Dir (n, dir) -> 
                  let childDirs =
                        dir
                        |> Seq.choose (function
                                       | Dir (n, d) -> Some (Dir (n,d))
                                       | _ -> None)
                        |> Seq.collect getDirs
                  Seq.append [Dir (n, dir)] childDirs
           
            
      let build = buildDir []
            

module Line = 
     let parse (line: string) =
        seq {
            yield (Regex.Match(line, "\$ cd (.+)").Groups)
                  |> Seq.skip 1
                  |> Seq.tryHead
                  |> Option.map (fun g -> g.Value)
                  |> Option.map (function | ".." -> GoUpDirectory | dir -> ChangeDirectory dir)
        
            yield if line = "$ ls" then Some Lis else None
              
            yield (Regex.Match(line, "dir (.+)").Groups)
                  |> Seq.skip 1
                  |> Seq.tryHead
                  |> Option.map (fun dirGroup -> DirLine dirGroup.Value)
              
            yield Regex.Matches(line, "(\d+) (.+)")
                  |> Seq.tryHead
                  |> Option.map (fun reg ->
                        reg.Groups
                        |> Seq.toArray
                        |> fun arr -> (FileLine (int arr.[1].Value, arr.[2].Value)))
        } |> Seq.choose id
        
     let parseOne = parse >> Seq.head

let sr = File.OpenText "../../../input.txt"

let lines = seq {
      while not sr.EndOfStream do
            yield sr.ReadLine()
}

let root =
      lines
      |> Seq.map Line.parseOne
      |> Seq.toList
      |> Dir.build
      |> fun (x, _) -> Dir ("\\", x)
    //  |> Dir.getDirs
    //  |> Seq.map Dir.getSize
   //   |> Seq.toList
   //   |> Seq.filter (fun x -> x <= 100000)
   //   |> Seq.sum

let rootSize = Dir.getSize root
let remainingSpace = 70000000 - rootSize

let spaceRequired = 30000000 - remainingSpace

let sizes =
      root
      |> Dir.getDirs
      |> Seq.choose (function | Dir (name, items) -> Some (name, (Dir.getSize (Dir (name, items)))) | _ -> None)
      |> Seq.filter (fun (n, d) -> d > spaceRequired)
      |> Seq.minBy (fun (n, d) -> d)
()