
let farmPlan = 
    System.IO.File.ReadLines("../data/21-test.txt") |> 
    Seq.toArray |> Array.map(fun x-> Seq.toArray x) |> array2D

let findChars theChar = 
    [0..farmPlan[0,*].Length - 1] |> List.mapi(fun i c-> 
        let startPos = farmPlan.[i,*] |> Array.tryFindIndex(fun x-> x=theChar)
        match startPos with
        | Some startPos -> [i;startPos]
        | _ -> [0;0]
    )

let startPos = 
     findChars 'S'|> List.reduce(fun acc x->if acc = [0;0] then [x.[0];x.[1]] else acc)

let rec moveStep (thePos:list<int>) theMax = 

    let moveList = [[thePos.[0]+1;thePos.[1]];[thePos.[0]-1;thePos.[1]];[thePos.[0]; thePos.[1]+1];[thePos.[0]; thePos.[1]-1]]

    match theMax with
    | -1 -> 1
    | _ ->

        moveList |> List.map(fun x->
        try
            //printfn "%A" farmPlan.[x.[0],x.[1]]
            //printfn "%d" (theMax-1)
            match farmPlan.[x.[0],x.[1]] with
            | c when not (c='#') && not (c='O') -> 
                farmPlan[thePos.[0],thePos.[1]] <- '.'
                farmPlan.[x.[0],x.[1]] <- 'O' 
                moveStep [x.[0];x.[1]] (theMax-1)
            | _ -> 0
        with
            | :? System.IndexOutOfRangeException -> 0
        
        ) |> List.sum

//not 1089, too low
printfn "%A" (moveStep startPos 6)
farmPlan.[startPos.[0],startPos.[1]] <- 'S'

printfn "%A" farmPlan 