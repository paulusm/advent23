open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("../data/24-test.txt")

let rxHail: Regex = Regex(@"\d+", RegexOptions.Compiled)

// stack overflow https://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }

let trajectories = inputLines |> Seq.map(fun x->
    let hailMatches = rxHail.Matches x
    hailMatches
) 

let trajCombinations = (combinations [] 2 (Seq.toList trajectories))

printfn "%A" (Seq.length trajCombinations)