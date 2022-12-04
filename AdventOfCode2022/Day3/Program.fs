// For more information see https://aka.ms/fsharp-console-apps

open System.Collections.Generic
open System.IO

let getPriority (item: char) =
    let baseNum = (System.Char.ToUpper(item) |> int) - 64
    if System.Char.IsUpper(item)
    then baseNum + 26
    else baseNum

let bagSorter = 
    use sr = File.OpenText "../../../input.txt"
    let mutable sum = 0
    while not sr.EndOfStream do
        let line = sr.ReadLine()
        let bag1, bag2 = (Seq.take (line.Length / 2) line), (Seq.skip (line.Length / 2) line)
        let hash = HashSet(bag1)
        let wrongItem = Seq.find hash.Contains bag2
        let priority = getPriority wrongItem
        sum <- sum + priority
    sum
    
let getBadges =
    use sr = File.OpenText "../../../input.txt"
    let ruckSacks = seq {
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }
    ruckSacks
    |> Seq.chunkBySize 3
    |> Seq.map (
        Array.map Set
        >> Set.intersectMany
        >> Set.minElement
        >> getPriority)
    |> Seq.sum

()
    