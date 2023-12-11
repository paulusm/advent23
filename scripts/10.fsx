open System.Text.RegularExpressions
open System

let theMap = System.IO.File.ReadLines("../data/10-test.txt") |> Seq.toList

let startPos:(int * int) = (theMap |> List.mapi(fun i x -> 
    let sPos = x.IndexOf("S")
    match sPos with
    | -1 -> (0,0)
    | _ -> (i,sPos)
) |> List.filter(fun x-> not(x=(0,0))) |> List.item 0)

let moves = [(1,0);(-1,0);(0,1);(0,-1)] 

// let findPath thePos:(int * int) = 
//     let x,y = thePos
//     moves |> List.map(fun m-> 
//         let movex, movey = m
//         let newPos = (x + movex, y + movey)
//         newPos
//     )

printfn "%A" startPos