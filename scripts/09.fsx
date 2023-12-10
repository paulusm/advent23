open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/09-test.txt") 

let rxSensorData: Regex = Regex(@"\-*\d+", RegexOptions.Compiled)

let sensorRows:seq<seq<seq<int>>> = (input |> Seq.map (fun x -> (rxSensorData.Matches x)) |> 
    Seq.map(fun x-> x |> Seq.map(fun y ->int y.Value)) |> 
    Seq.map(fun x-> seq{x}))

let rec computeDiff (theRow:seq<seq<int>>):seq<seq<int>> = 
    let resultRow = theRow |> Seq.last |> Seq.pairwise |> Seq.map(fun s -> (snd s) - (fst s))
    let zeroCheck = resultRow |> Seq.map(fun x -> abs(x) ) |> Seq.sum 
    let resultSeq = seq {resultRow}
    match zeroCheck with
     | 0 -> Seq.append theRow resultSeq
     | _ -> Seq.append theRow (computeDiff resultSeq)

let processSeqs:seq<seq<seq<int>>> =
    sensorRows |> Seq.map(fun x -> //x is our sequence of transformations
        let theNextRow:seq<seq<int>> = computeDiff (x)
        theNextRow
    )

// Part One

let predictSeqs:int = 
    processSeqs |> Seq.map(fun x->
        //printfn "%A" x
        // x is each <seq<seq<int>>
        x|> Seq.rev |> Seq.map(fun y->
            // y is each <seq<int>>
           // printfn "%d %A" i (Seq.rev y)
            printfn " - last item %A" (y |> Seq.rev |> Seq.item 0)
            y |> Seq.item 0) |> Seq.sum //y |> Seq.rev for part 1
    ) |> Seq.sum

printfn "Part One %A" (predictSeqs) 
