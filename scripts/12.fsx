 
open System.Text.RegularExpressions
let inputData = System.IO.File.ReadLines("../data/12-test.txt") 

let rec runMatches  (theString:string) (theRegex:Regex) = 
    let springMatches = theRegex.Matches theString
    printfn "matches = %d" (Seq.length springMatches)
    let matchSeq:seq<string> = springMatches |> Seq.mapi(fun i (theMatch:Match)->
        theMatch.Groups.[i+1].Value
    )
    matchSeq

let result = inputData |> Seq.map(fun x ->
    let springLine = x.Split ' '
    let seqParts = springLine.[1].Split ','
    let reTestString = (seqParts |> Array.mapi(fun i seqLen ->
        @".*([#|?]{" + seqLen + "})"+if i < (Seq.length seqParts)-1 then "[^#][?|.]*" else "(?=[^#]|$)"
    ) |> Array.reduce(fun acc x -> acc + x))
    printfn "re = %s" reTestString
    let reTest =  Regex (reTestString) //+ 
    runMatches springLine.[0] reTest)

printfn "%A" (result |> Seq.map(fun x-> x))
