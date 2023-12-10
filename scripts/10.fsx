open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/10-test.txt") |> Seq.toList

printfn "%A" input
