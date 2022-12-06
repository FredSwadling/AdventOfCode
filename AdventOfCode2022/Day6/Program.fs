// For more information see https://aka.ms/fsharp-console-apps

open System.IO

printfn "Hello from F#"

use sr = File.OpenText "../../../input.txt"
let x =
    sr.ReadLine()
    |> Seq.windowed 14
    |> Seq.takeWhile (fun window -> (Array.distinct window).Length <> 14)
    |> Seq.toList
    
let y = x.Length + 14
()
