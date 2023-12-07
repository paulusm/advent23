open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/07-test.txt") 

let rxData: Regex = Regex(@"[\dTJKQKA]+", RegexOptions.Compiled)

let sortHand (s:char):int =
    match s with 
    | 'A' -> 0
    | 'K' -> 1
    | 'Q' -> 2
    | 'J' -> 3
    | 'T' -> 4
    | _ -> 10-((int(s))-52)

let scoreHand (row:MatchCollection):int = 
    let handMatch:Match = row |> Seq.item 0
    let hand:string = handMatch.Value
    let handSorted:string = hand.ToCharArray() |> Array.sortBy(fun s -> sortHand s) |> System.String
    printfn "%s" handSorted
    1

let result = input |> Seq.map(fun x -> rxData.Matches(x)) |> Seq.sortBy (fun s -> scoreHand s)

printfn "%A" result