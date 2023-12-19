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

let rec scanSegment (theSegment:int[]) (theStartPos:int)  (theRow:int) =
    let fromEdge = theSegment |> Array.tryFindIndex(fun x->x=1)    
    match fromEdge with
    | Some a -> 
        let toEdge = theSegment[a+1..] |> Array.tryFindIndex(fun x->x=1)
        match toEdge with
        | Some b ->
            //printfn "segment %A" theSegment
            //printfn "from %d to %d" a (b+a)
            ([a..(b+a)] |> List.map (
                fun y ->
                    lavaPlan[theRow,y+theStartPos]<-1
                    1
            )|> List.sum  ) +  scanSegment (theSegment.[b + a + 1..]) (b + a + 1) theRow
        | _ -> 1
    | _ -> 0

let fillMiddle =
    //[0..planSize-1] 
    [0..planSize-1]   |> List.map (fun x-> 
        //printfn "number of edges at row %d %d" x (lavaPlan[x,*] |> Array.filter(fun x->x=1)|> Array.length)
        // TODO: cant just fill from first to last, as multiple parts
        let segment = lavaPlan[x,*]
        let lavaCount = scanSegment segment 0 x
        lavaCount
    )  


parsedPlan
//59998 too high 
printfn "%A" (fillMiddle |> List.sum)

//rintfn "%A" (lavaPlan)