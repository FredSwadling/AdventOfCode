// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Text.RegularExpressions
        
let mapIndexToTrueIndex index = 
    (index / 4) + 1
    
let toBoxLine (line: string) =
    Regex.Matches(line, "\[\w\]")
    |> Seq.map (fun c -> mapIndexToTrueIndex c.Index, c.Value)
    |> Seq.toList
    
let parseInstruction (line: string) =
    let groups = Regex.Match(line, "move (\d+) from (\d+) to (\d+)").Groups |> Seq.toArray
    groups.[1].Value |> int,
    groups.[2].Value |> int,
    groups.[3].Value |> int
    
let toModel lines =
    let addBoxToModel model (index, box) =
        let currentSet = Map.find index model
        Map.add index (box::currentSet) model
        
    let addLayerToModel layer model =
        Seq.fold addBoxToModel model layer
        
    [1..10]
    |> List.map (fun index -> index, [])
    |> Map
    |> Seq.foldBack addLayerToModel lines
    
let applyStep1 model (from, too) =
        let fromColumn, toColumn = Map.find from model, Map.find too model
        match fromColumn with
        | [] -> model
        | fromH::fromColumn ->
            model
            |> Map.add from fromColumn
            |> Map.add too (fromH::toColumn)

let applyInstruction1 model (amount, from, too) = 
    [1..amount]
    |> List.map (fun _ -> (from, too))
    |> List.fold applyStep1 model
      
let applyInstruction2 model (amount, from, too) =
    let fromColumn, toColumn = Map.find from model, Map.find too model
    let taken, leftover = List.truncate amount fromColumn, fromColumn |> List.skip amount
    model
    |> Map.add from leftover
    |> Map.add too (taken @ toColumn)
    
let ans =
    use sr = File.OpenText "../../../input.txt"
    let lineSeq =
        seq {
            while not sr.EndOfStream do
                yield sr.ReadLine()
        }
    let boxes =
        lineSeq
        |> Seq.map toBoxLine
        |> Seq.takeWhile (function | [] -> false | _ -> true)
        |> Seq.toList
        |> toModel
        
    sr.ReadLine() |> ignore
    
    let afterInstructions =
        lineSeq
        |> Seq.map parseInstruction
        |> Seq.fold applyInstruction2 boxes
        
    let tops =
        afterInstructions
        |> Seq.map (fun x -> match x.Value with | h::t -> h | [] -> "*")
        |> Seq.toList
    tops
        
    