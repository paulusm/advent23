open System.Text.RegularExpressions
open System

let theMap = System.IO.File.ReadLines("../data/10-test.txt") |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) |> array2D 

let moves = [[1;0];[-1;0];[0;1];[0;-1]] 

let startPos = [0;1]

let possMoves (theStartPos:list<int>) = 
    moves |> List.map(fun x->
        let possPos = [theStartPos.[0] +  x.[0]; theStartPos.[1] +  x.[1]]
        possPos
    ) |> List.filter(fun x-> x.[0] > -1 && x.[1] > -1)

let rec moveCount (thePos:list<int>)  =
    let moveOptions = possMoves thePos
    moveOptions |> List.map(fun x ->
        printfn $"Move %s{(theMap.[x.[0],x.[1]]).ToString()}"
        x
    )

printfn "%A" (moveCount startPos)