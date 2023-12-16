open System.Text.RegularExpressions

let inputData = System.IO.File.ReadAllText("../data/15-test.txt") 

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
let boxMap = Map.empty

let lensOp theBox theOp theLens theVal :Map<string,string> = 
    let lensMap = if boxMap.ContainsKey theBox then boxMap[theBox] else Map.empty
    match theOp with
    | "-" -> lensMap.Remove theLens
    |  _  -> lensMap.Add(theLens,theVal)

let part2Result:Map<int,Map<string,string>>[] = 
    steps |> Array.map(fun step ->
    let matchStep = rxInstruction.Match step
    let boxNum = boxValue matchStep.Groups.[1].Value
    let boxContents:Map<string, string> = lensOp boxNum matchStep.Groups.[2].Value matchStep.Groups.[1].Value matchStep.Groups.[3].Value
    if boxMap.ContainsKey boxNum then 
        Map.remove boxNum boxMap |> Map.add boxNum boxContents 
    else 
        Map.add boxNum boxContents boxMap)
    

printfn "Part 2 - %A" (part2Result)