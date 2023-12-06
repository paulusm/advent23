open System.Text.RegularExpressions
open System.Text


let input = System.IO.File.ReadLines("../data/06.txt") 

//part One

let rxData: Regex = Regex(@"\d+", RegexOptions.Compiled)


// For Part One
// let result = input |> Seq.map(fun x->
//     rxData.Matches(x)
// ) 

// For Part Two
let result = input |> Seq.map(fun x->
    let fullString = x.Replace(" ","")
    rxData.Matches(fullString)
) 

let evaluateOptions = ((Seq.item 0 result),(Seq.item 1 result)) ||> Seq.map2 (fun time distance ->
    let t = int64(time.Value)
    let d = int64(distance.Value)
    [0L..t] |> List.map(fun b -> 
    let dist = (t-b) * b 
    match dist with 
    | dist when dist > d -> dist
    | _ -> 0L
    ) |> Seq.filter (fun x-> x>0L)) |> Seq.map (fun x->Seq.length x)

printfn "%A" (evaluateOptions |> Seq.reduce(fun acc x -> acc * x))


