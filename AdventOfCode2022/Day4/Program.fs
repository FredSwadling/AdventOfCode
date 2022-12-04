// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Text.RegularExpressions

let subsumes (a,b) (c,d) =
    a <= c && b>=d

let overlaps (a,b) (c,d) =
    let regions =
        [(a, "("); (b, ")"); (c, "{"); (d, "}")]
        |> Seq.groupBy fst 
        |> Seq.map (fun (key, x) -> key, (x |> Seq.map snd |> Set))
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.toArray
    
    let mutable overlapped = false
    let mutable isAOpen = false
    let mutable isBOpen = false
   
    for x in regions do
        if x.Contains("(") then isAOpen <- true
        if x.Contains("{") then isBOpen <- true
        if (isAOpen && isBOpen) then overlapped <- true
        if x.Contains(")") then isAOpen <- false
        if x.Contains("}") then isBOpen <- false
        
    overlapped
   
    
let ans =
    use sr = File.OpenText "../../../input.txt"
    seq {
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }
    |> Seq.map (fun line -> line.Split(","))
    |> Seq.map (fun line -> Regex.Match(line.[0], "(\d+)-(\d+)"), Regex.Match(line.[1], "(\d+)-(\d+)"))
    |> Seq.map (fun (regexa, regexb) ->
        (((regexa.Groups.Item 1).Value |> int, (regexa.Groups.Item 2).Value |> int),
         ((regexb.Groups.Item 1).Value |> int, (regexb.Groups.Item 2).Value |> int)))
    |> Seq.filter(fun (a, b) -> overlaps a b)
    |> Seq.toList
    
()