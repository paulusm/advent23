open System.Text.RegularExpressions
open System

let theImageRaw = System.IO.File.ReadLines("../data/11-test.txt") |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) 
let theImage = array2D theImageRaw

let theImageDim = Array.length theImage.[0,*]-1

let rowBlank = [0..theImageDim] |> List.map(fun x ->
    theImage.[x,*] |> Array.exists(fun x ->
    x='#'
    )
)


printfn "%A" rowBlank