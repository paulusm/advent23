open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("../data/04.txt")

let rxPart: Regex = Regex(@"\d+", RegexOptions.Compiled)

let allParts = inputLines |> Seq.map(fun dataLine -> 
  let cardParts = dataLine.Substring(9).Split("|")
  let winnumMatches:MatchCollection = cardParts.[0] |> rxPart.Matches
  let mynumMatches:MatchCollection = cardParts.[1] |> rxPart.Matches
  let wins:seq<string> = winnumMatches |> Seq.map(fun x -> x.Value) |> Seq.filter(fun x -> mynumMatches |> Seq.exists(fun y->y.Value=x) )
  let wincount:seq<int>= wins |> Seq.map(fun x-> int x) 
  let winnum:int = wincount |> Seq.length
  winnum
) 
let tot = allParts |> Seq.filter(fun x-> x>0) |> Seq.map(fun x -> 2.0 ** float(x-1) ) |> Seq.sum
printfn "Part One - %A" tot