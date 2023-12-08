open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/07.txt") 

let rxData: Regex = Regex(@"[\dTJKQKA]+", RegexOptions.Compiled)


// Part One 
let cardREList: List<string> = ["A";"K";"Q";"J";"T";"9";"8";"7";"6";"5";"4";"3";"2"] 


let getSubsort (row:MatchCollection):int64  = 
    let handMatch:Match = row |> Seq.item 0
    let hand:string = handMatch.Value
    let indexScore =  ([0..4] |> List.map (fun i ->
      int64(string(12  - (cardREList |>  List.findIndex(fun x-> x = hand.Substring(i,1)))) + String.replicate (4-i) "00")
    ) |> List.sum )
    indexScore

let buildSortNum (from:int) :int64 =
    (int64(string from + String.replicate 12 "0"))

let scoreHand (row:MatchCollection):int64 = 
    let handMatch:Match = row |> Seq.item 0
    let hand:string = handMatch.Value
    let cardMatches = (cardREList |> List.map(fun x ->
        let cardRE: Regex = Regex x
        let cardMatches:MatchCollection = cardRE.Matches hand
        let m = cardMatches |> Seq.length
        int64(m)
        ) |> List.sortBy(fun cardCount ->
            5L - cardCount
        ) |> List.reduce(fun acc x ->
            match acc with 
            | 5L -> buildSortNum 7
            | 4L -> buildSortNum 6
            | 3L -> if x = 2L then buildSortNum 5 else buildSortNum 4
            | 2L -> if x = 2L then buildSortNum 3 else buildSortNum 2
            | 1L -> buildSortNum 1
            | _  -> acc 
        ) 
    ) 
    //printfn "Hand  %s Matches %A" hand (cardMatches + getSubsort row)
    cardMatches  + getSubsort row


let result = 
    input |> Seq.map(fun x -> rxData.Matches(x)) |> 
    Seq.sortBy (fun s -> scoreHand s) |>
    Seq.mapi (fun i x -> 
        //printfn "%s" x.[0].Value
        (i+1) * int(x.[1].Value)) |>
    Seq.sum

printfn "Part 1 = %A" result


// Part Two - Something Wrong here
let cardREList2: List<string> = ["A";"K";"Q";"T";"9";"8";"7";"6";"5";"4";"3";"2";"J"] 

let getSubsort2 (row:MatchCollection):int64  = 
    let handMatch:Match = row |> Seq.item 0
    let hand:string = handMatch.Value
    let indexScore =  ([0..4] |> List.map (fun i ->
      int64(string(12  - (cardREList2 |>  List.findIndex(fun x-> x = hand.Substring(i,1)))) + String.replicate (4-i) "00")
    ) |> List.sum )
    indexScore

let buildSortNum2 (from:int) :int64 =
    (int64(string from + String.replicate 12 "0"))

let scoreHand2 (row:MatchCollection):int64 = 
    let handMatch:Match = row |> Seq.item 0
    let hand:string = handMatch.Value
    let cardMatches = (cardREList |> List.map(fun x ->
        let cardRE: Regex = Regex ("[" + x + "|J]")
        let cardMatches:MatchCollection = cardRE.Matches hand
        let m = cardMatches |> Seq.length
        int64(m)
        ) |> List.sortBy(fun cardCount ->
            5L - cardCount
        ) |> List.reduce(fun acc x ->
            match acc with 
            | 5L -> buildSortNum2 7
            | 4L -> buildSortNum2 6
            | 3L -> if x = 2L then buildSortNum2 5 else buildSortNum 4
            | 2L -> if x = 2L then buildSortNum2 3 else buildSortNum 2
            | 1L -> buildSortNum2 1
            | _  -> acc 
        ) 
    ) 
    //printfn "Hand  %s Matches %A" hand (cardMatches + getSubsort2 row)
    cardMatches  + getSubsort2 row


let result2 = 
    input |> Seq.map(fun x -> rxData.Matches(x)) |> 
    Seq.sortBy (fun s -> scoreHand2 s) |>
    Seq.mapi (fun i x -> 
        //printfn "%s" x.[0].Value
        (i+1) * int(x.[1].Value)) |>
    Seq.sum

printfn "Part 2 = %A" result2