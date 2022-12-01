// For more information see https://aka.ms/fsharp-console-apps

open System
open System.Collections.Generic
open System.IO
let countCalories = 
    use streamReader = File.OpenText "../../../input.txt"
    let mutable max = 0
    let mutable currentSum = 0
    while not streamReader.EndOfStream do
        let line = streamReader.ReadLine()
        if String.IsNullOrWhiteSpace line
        then
            max <- Math.Max(max, currentSum)
            currentSum <- 0
        else
            let number = line |> int
            currentSum <- currentSum + number
    max
    
let countTopThreeCalories = 
    use streamReader = File.OpenText "../../../input.txt"
    let mutable sums = SortedSet<int>()
    let mutable currentSum = 0
    while not streamReader.EndOfStream do
        let line = streamReader.ReadLine()
        if String.IsNullOrWhiteSpace line
        then
            do sums.Add(currentSum) |> ignore
            currentSum <- 0
        else
            let number = line |> int
            currentSum <- currentSum + number
    sums.Reverse()
    |> Seq.truncate 3
    |> Seq.sum
    
()
