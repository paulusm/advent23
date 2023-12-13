open System.Text.RegularExpressions
open System

let theMap = System.IO.File.ReadLines("../data/10-test.txt") |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) |> array2D 

let moves = [[1;0];[-1;0];[0;1];[0;-1]] 

let startPos = [0;1]

let possMoves (theStartPos:list<int>) (thePathHistory:list<list<int>>) = 
    moves |> List.map(fun x->
        let possPos = [theStartPos.[0] +  x.[0]; theStartPos.[1] +  x.[1]]
        possPos
    ) |> List.filter(fun x-> x.[0] > -1 && x.[1] > -1 && not(List.contains theStartPos thePathHistory))

let rec moveCount (thePos:list<int>) (pathHist:list<list<int>>)  =
    let pathHistRet = List.insertAt 0 thePos pathHist
    let moveOptions = possMoves thePos pathHist
    moveOptions |> List.map(fun x ->
        let moveChar = theMap.[x.[0],x.[1]]
        printfn $"Move %s{(moveChar).ToString()}"
        match moveChar with
        | '.' -> pathHistRet
        | _ ->  List,insertAt 0 moveCount x pathHistRet
    ) 

printfn "%A" (moveCount startPos [])