open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/09-test.txt") 

let rxSensorData: Regex = Regex(@"\d+", RegexOptions.Compiled)

let sensorRows:seq<seq<seq<int>>> = (input |> Seq.map (fun x -> (rxSensorData.Matches x)) |> 
    Seq.map(fun x-> x |> Seq.map(fun y ->int y.Value)) |> 
    Seq.map(fun x-> seq{x}))

let rec computeDiff (theRow:seq<seq<int>>):seq<seq<int>> = 
    let resultRow = theRow |> Seq.last |> Seq.pairwise |> Seq.map(fun s -> (snd s) - (fst s))
    let zeroCheck = resultRow |> Seq.sum 
    let resultSeq = seq {resultRow}
    match zeroCheck with
     | 0 -> resultSeq
     | _ -> Seq.append theRow (computeDiff resultSeq)

let processSeqs:seq<seq<seq<int>>> =
    sensorRows |> Seq.map(fun x -> //x is our sequence of transformations
        let theNextRow:seq<seq<int>> = computeDiff (x)
        theNextRow
    )

printfn "%A" (processSeqs |> Seq.item 0 |> Seq.item 1)