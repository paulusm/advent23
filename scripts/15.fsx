open System.Text.RegularExpressions

let inputData = System.IO.File.ReadAllText("../data/15.txt") 

let steps = inputData.Split(',')

// Part One

let stepValue cumTotal theStep = 
    theStep |> Seq.mapi (fun i theChar -> 
            if i=0 then int theChar * 17 % 256 else int theChar 
    ) |> Seq.reduce(fun acc charVal-> ((acc + charVal) * 17) % 256)

let result = steps |> Array.map(fun step ->
    stepValue 0 step
) 


printfn "Part 1 - %A" (result |> Array.sum)

// Part Two

let boxValue theStep = 
    theStep |> Seq.mapi (fun i theChar -> 
            if i=0 then int theChar * 17 % 256 else int theChar 
    ) |> Seq.reduce(fun acc charVal-> ((acc + charVal) * 17) % 256)

let rxInstruction = Regex(@"([a-z]+)([=-])(\d*)", RegexOptions.Compiled)
//let boxMap = Map.empty

let doRemove (lensMap:Map<int,string* string>) (theLens:string) = 
    let mapKeys = lensMap.Keys
    if mapKeys |> Seq.length = 0 then 
        lensMap
    else
        let theKey = (mapKeys |> Seq.map(fun x->
            let (lensName,lensVal) = lensMap[x]
            if lensName = theLens then x else 0) |> Seq.max)
        lensMap.Remove theKey 

let doAdd (lensMap:Map<int,string* string>) (theNewKey:int) (theNewVal:string * string)=
    let mapKeys = lensMap.Keys
    let (newLens, newVal) = theNewVal
    let theKey = 
        if mapKeys.Count = 0 then
             0
        else
            mapKeys |> Seq.map(fun x->
                let (lensName,lensVal) = lensMap[x]
                if lensName = newLens then x else 0) |> Seq.max
    let insKey = if theKey=0 then theNewKey else theKey
    lensMap.Add (insKey,theNewVal)
   

let lensOp (theBoxMap:Map<int,Map<int,string * string>>) theBox theOp (theLens:string) (theVal:string) :Map<int,string * string> = 
    let lensMap = if theBoxMap.ContainsKey theBox then theBoxMap[theBox] else Map.empty
    //printfn "Box %d already has %d lenses" theBox lensMap.Count
    let nextKey = if lensMap.Count>0 then lensMap.Keys |> Seq.max else 0
    match theOp with
    | "-" -> doRemove lensMap theLens
    |  _  -> doAdd lensMap (nextKey+1) (theLens,theVal)

let boxMap:Map<int,Map<int,string * string>> = 
    (Map.empty, steps) ||> Array.fold(fun acc step ->
    let matchStep = rxInstruction.Match step
    let boxNum = boxValue matchStep.Groups.[1].Value
    let boxContents:Map<int, string * string> = lensOp acc boxNum matchStep.Groups.[2].Value matchStep.Groups.[1].Value matchStep.Groups.[3].Value
    //printfn "%A" boxContents
    if acc.ContainsKey boxNum then 
        Map.remove boxNum acc |> Map.add boxNum boxContents 
    else 
        Map.add boxNum boxContents acc)

let result2 = 
    boxMap |> Map.keys |> Seq.map(fun x->
    let theLensMap = boxMap[x]
    let theLensScore = 
        theLensMap |> Map.values |> Seq.mapi(fun i y->
            //printfn "%A" y
            let(lens,focal) = y
            (i+1) * int focal
        ) |> Seq.sum
    theLensScore * (x+1) 
    ) |> Seq.sum

printfn "Part 2 - %A" (result2)