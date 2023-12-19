open System.Text.RegularExpressions

let planRaw:seq<string> = System.IO.File.ReadLines("../data/18.txt")

//printfn "%d lines read" (planRaw |> Seq.length)
let rxData: Regex = Regex(@"([RDLU]) (\d+) \((#[0-9a-z]{6})\)", RegexOptions.Compiled)

let planSize = 440 //10

let lavaPlan = Array2D.init planSize planSize (fun x y->0)

let mutable initPos = (210,50) //(0,0)

let createPath (moveX, moveY) startPos =
    let (initX, initY) = startPos
    //printfn "%d %d -> %d %d" initX initY (initX + moveX) (initY + moveY)
    let pathAdd = 
        match moveX with
        | 0 -> (if moveY > 0 then [initY..initY+moveY] else [initY+moveY..initY]) |> List.map(fun y->lavaPlan[initX, y]<-1) 
        | _ -> (if moveX > 0 then[initX..initX+moveX] else [initX+moveX..initX])|> List.map(fun x->lavaPlan[x, initY]<-1) 
    (initX + moveX, initY + moveY)

let parsedPlan = 
    planRaw |> Seq.toList |> List.map(fun x->
        let planElements = x |> rxData.Match 
        let digDirection = planElements.Groups.[1].Value 
        let digDistance = int(planElements.Groups.[2].Value)
        let digColour =  planElements.Groups.[3].Value
        let newPos = 
            match digDirection with
            | "D" -> createPath (digDistance, 0) initPos
            | "U" -> createPath ( - digDistance,0) initPos
            | "R" -> createPath (0,  + digDistance) initPos
            | "L" -> createPath (0,  - digDistance) initPos
            | _ -> (0,0)
        initPos <- newPos
        initPos
    )

let isEnclosed a b :bool = 
    let n = [0..planSize-b-1] |> List.map(fun i->if lavaPlan.[a,b+i]=1  then  true else false) |> List.reduce(fun acc x-> acc || x)
    let s = [0..b] |> List.map(fun i->if lavaPlan.[a,b-i]=1  then  true else false) |> List.reduce(fun acc x-> acc || x)
    let e = [0..planSize-a-1] |> List.map(fun i->if lavaPlan.[a+i,b]=1  then  true else false) |> List.reduce(fun acc x-> acc || x)
    let w = [0..a] |> List.map(fun i->if lavaPlan.[a-i,b]=1  then  true else false) |> List.reduce(fun acc x-> acc || x)
    n && s && w && e 


let fillMiddle =
    //[0..planSize-1] 
    [0..planSize-1]   |> List.map (fun x->
        [0..planSize-1]   |> List.map (fun y-> 
            match isEnclosed x y with
            | true -> 1
            | false ->0
        ) |> List.sum
    )  |> List.sum

//59998 too high 56751 too high. Not 55518
printfn "%A" (fillMiddle)

//rintfn "%A" (lavaPlan)