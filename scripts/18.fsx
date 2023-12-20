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

let mutable edgeLength = []

let parsedPlan = 
    planRaw |> Seq.toList |> List.map(fun x->
        let planElements = x |> rxData.Match 
        let digDirection = planElements.Groups.[1].Value 
        let digDistance = int(planElements.Groups.[2].Value)
        edgeLength <-edgeLength |> List.insertAt 0 digDistance
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

let rec fillMiddle startX startY =
    match lavaPlan.[startX, startY] with
    | 1 -> 0
    | 2 -> 0
    | _ ->
        lavaPlan.[startX,startY] <- 2
        1 + fillMiddle (startX+1) startY + fillMiddle startX (startY+1) + fillMiddle (startX-1) startY + fillMiddle startX (startY-1)
        

printfn "%d" (fillMiddle 200 200 + List.sum edgeLength)

// Just draw the plan for debugging purposes
let theLines:string[] = [|0..planSize-1|] |> Array.map(fun x ->
    let theLine:string = lavaPlan.[x,*] |> Array.map(fun x ->string x) |> String.concat ""
    theLine
)
System.IO.File.WriteAllLines ("../data/18-plan-filled.txt", theLines)
