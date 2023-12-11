open System.Text.RegularExpressions
open System

let my2DArray = array2D [ [ 1; 0]; [0; 1] ]
let theImageRaw = System.IO.File.ReadLines("../data/11-test.txt") |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) 
let theImage = array2D theImageRaw

printfn "%A" (theImage.[3,3])