open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/09-test.txt") 

let rxSensorData: Regex = Regex(@"\d+", RegexOptions.Compiled)

let sensorRows = input |> Seq.map (fun x -> rxSensorData.Matches x) 

let computeDiff = sensorRows |> Seq.map(fun x ->
    x |> Seq.pairwise |> Seq.map(fun a -> int (snd a).Value - int (fst a).Value)
)


printfn "%A" (Seq.item 0 computeDiff |> Seq.iter (printfn "%A"))