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

let rec isOutside a b move:int = 

    let moveX, moveY = move
    try
        let theScore = if lavaPlan.[a+moveX,b+moveY]=0 then lavaPlan.[a,b+1] <- 2; 1 else 0
        match theScore with
        | 0 -> 0
        | 1 -> 1 + isOutside  (a+moveX) (b+moveY) (0,1) + 
                isOutside (a+moveX) (b+moveY) (1,0) +
                isOutside (a+moveX) (b+moveY) (0,-1) +
                isOutside (a+moveX) (b+moveY) (-1,0) 
    with 
    | :? System.IndexOutOfRangeException -> 0 
    

let floodOutside =
    isOutside 0 0 (1,0)



//59998 too high 56751 too high. Not 55518
printfn "%A" (planSize * planSize - floodOutside)

let theLines:string[] = [|0..planSize-1|] |> Array.map(fun x ->
    let theLine:string = lavaPlan.[x,*] |> Array.map(fun x ->string x) |> String.concat ""
    theLine
)
System.IO.File.WriteAllLines ("../data/18-plan.txt", theLines)
