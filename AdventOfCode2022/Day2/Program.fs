// For more information see https://aka.ms/fsharp-console-apps

open System.IO

type Hand =
    | Rock
    | Paper
    | Scissors
    
module Hand =
    let parseChar = function
        | 'A' | 'X' -> Rock
        | 'B' | 'Y' -> Paper
        | 'C' | 'Z' -> Scissors
        | _ -> failwith "Whatever"
        
    let getScore = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
        
type Outcome =
    | Win
    | Draw
    | Lose
    
module Outcome =
    let getScore = function
        | Win -> 6
        | Draw -> 3
        | Lose -> 0
        
    let parseChar = function
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith "Whatever"
    
type Round = Hand * Hand

module Round =
         
    let getOutcome = function
        | Rock, Scissors
        | Scissors, Paper
        | Paper, Rock -> Lose
        | Rock, Rock
        | Scissors, Scissors
        | Paper, Paper -> Draw
        | Rock, Paper
        | Paper, Scissors
        | Scissors, Rock -> Win
        
    let forOutcomeGetHand = function
        | x, Draw -> x
        | Rock, Lose -> Scissors
        | Rock, Win -> Paper
        | Scissors, Lose -> Paper
        | Scissors, Win -> Rock
        | Paper, Lose -> Rock
        | Paper, Win -> Scissors
           
    let getScore = getOutcome >> Outcome.getScore
    
let strategy1 (line: string) =
    let _, hand2 as round = Hand.parseChar line.[0], Hand.parseChar line.[2]
    (Hand.getScore hand2) + (Round.getScore round)

let strategy2 (line: string) =
    let hand1, outcome = Hand.parseChar line.[0], Outcome.parseChar line.[2]
    let hand2 = Round.forOutcomeGetHand (hand1, outcome)
    (Hand.getScore hand2) + (Round.getScore (hand1, hand2))
      
let score strategy = seq {
    use sr = File.OpenText "../../../input.txt"
    while not sr.EndOfStream do
        let line = sr.ReadLine()
        yield strategy line
}

let theoreticalBestScore =
    score strategy1
    |> Seq.sum
    
let newBestScore =
    score strategy2
    |> Seq.sum
()

