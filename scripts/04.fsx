open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("../data/04-test.txt")

let rxPart: Regex = Regex(@"\d+", RegexOptions.Compiled)

//Part One

let allParts = inputLines |> Seq.map(fun dataLine -> 
  let cardParts = dataLine.Substring(9).Split("|")
  let winnumMatches:MatchCollection = cardParts.[0] |> rxPart.Matches
  let mynumMatches:MatchCollection = cardParts.[1] |> rxPart.Matches
  // This finds common numbers in the two match sequences
  let wins:seq<string> = winnumMatches |> Seq.map(fun x -> x.Value) |> Seq.filter(fun x -> mynumMatches |> Seq.exists(fun y->y.Value=x) )
  let wincount:seq<int>= wins |> Seq.map(fun x-> int x) 
  let winnum:int = wincount |> Seq.length
  winnum
) 
let tot = allParts |> Seq.filter(fun x-> x>0) |> Seq.map(fun x -> 2.0 ** float(x-1) ) |> Seq.sum
printfn "Part One - %A" tot

//Part Two


let rec matchCount (i:int) (dataLine:string) =
  let cardParts = dataLine.Substring(9).Split("|")
  let winnumMatches:MatchCollection = cardParts.[0] |> rxPart.Matches
  let mynumMatches:MatchCollection = cardParts.[1] |> rxPart.Matches
  // This finds common numbers in the two match sequences
  let wins:int = winnumMatches |> Seq.map(fun x -> x.Value) |> Seq.filter(fun x -> mynumMatches |> Seq.exists(fun y->y.Value=x) ) |> Seq.length
  if wins > 1 then 
    //let recurse = [0..wins-1] |> List.map(fun x -> matchCount (i + x) (inputLines |> Seq.item x)) 
    wins
  else
    1

let allCards = inputLines |> Seq.mapi(fun i dataLine -> 
  matchCount i dataLine
) 

printfn "Part Two- %A" allCards