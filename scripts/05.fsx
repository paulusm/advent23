open System.Text.RegularExpressions
open System.Text


let input = System.IO.File.ReadAllText("../data/05.txt") 

// Key: the destination range start, the source range start, and the range length.

let rxData: Regex = Regex(@"\d+", RegexOptions.Compiled)

let sections = (Regex.Split(input,@":" ) |> 
     Array.map(fun x-> x |> rxData.Matches) |> 
     Array.filter(fun x -> x |> Seq.length > 0 ))

// Let's convert to big int for convenience
let lookups = sections |> Array.map(fun x -> x |> Seq.map (fun y-> int64(y.Value)))

let lookup x theSeq = 
     let result=(theSeq |> Seq.chunkBySize 3  |> 
               Seq.map(fun (z:array<int64>)->
               //Triple of dest, source and range length z
                (if x < z.[1] + z.[2] && x >= z.[1] then x - z.[1] + z.[0] else 0L)
               ) |> Seq.sum )
     match result with
          | 0L -> x
          | _ -> result

let seeds = lookups.[0]

// TODO refactor this abomination :angry:😮
let result = (seeds |> 
     Seq.map(fun x ->lookup x lookups.[1]) |>
     Seq.map(fun x ->lookup x lookups.[2]) |>
     Seq.map(fun x ->lookup x lookups.[3]) |>
     Seq.map(fun x ->lookup x lookups.[4]) |>
     Seq.map(fun x ->lookup x lookups.[5]) |>
     Seq.map(fun x ->lookup x lookups.[6]) |>
     Seq.map(fun x ->lookup x lookups.[7])
)

printfn "%A" (result |> Seq.min) 

// Part Two

let seeds2chunks = lookups.[0] |> Seq.chunkBySize(2) 

     Seq.map(fun x ->[x.[0]..(x.[0] + x.[1] - 1L)])) //|> 
     //Seq.toList |> List.concat)

// TODO refactor this abomination :angry:😮
//let result2 = (seeds2 |> 
     //Seq.map(fun x ->lookup x lookups.[1]) |>
     // Seq.map(fun x ->lookup x lookups.[2]) |>
     // Seq.map(fun x ->lookup x lookups.[3]) |>
     // Seq.map(fun x ->lookup x lookups.[4]) |>
     // Seq.map(fun x ->lookup x lookups.[5]) |>
     // Seq.map(fun x ->lookup x lookups.[6]) |>
     // Seq.map(fun x ->lookup x lookups.[7])
//)

printfn "%A" (seeds2 )//|> Seq.min) ß