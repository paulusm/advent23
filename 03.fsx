open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("03.txt")

// Part One

let rxPart: Regex = Regex(@"\d+", RegexOptions.Compiled)
let rxSymbol: Regex = Regex(@"[^\.\d]{1}", RegexOptions.Compiled)

let parseSchematic engineLine (theRegex:Regex):MatchCollection = 
    let itemsFound: MatchCollection  =  theRegex.Matches engineLine 
    itemsFound

let allParts = inputLines |> Seq.map(fun dataLine -> parseSchematic dataLine rxPart) 
let allSymbols:List<MatchCollection> = Seq.toList (inputLines |> Seq.map(fun dataLine -> parseSchematic dataLine rxSymbol))

let checkAdjacency (thePart:Match) i:int = 
    let theSymbolMatch = allSymbols |>  List.item i 

    if Seq.isEmpty theSymbolMatch then
      0
    else
        theSymbolMatch |> Seq.map (fun x->
            if (thePart.Index + thePart.Length) - x.Index < (thePart.Length + 2) && (thePart.Index + thePart.Length) - x.Index >=0 then 
                1
            else 
                0
        ) |>  Seq.reduce (fun acc x -> acc + x) 


let getValidParts  i (partsLine:MatchCollection):int =
 
    let sumValid:int = partsLine |> Seq.map( fun part -> 
            let symbolAdjacent:int = checkAdjacency part i + (if i > 0 then checkAdjacency part (i-1) else 0) + (if i < (Seq.length allParts-1) then checkAdjacency part (i+1) else 0)
            match symbolAdjacent  with
            | 0 -> 0
            | _ -> int part.Value) |> Seq.sum
    sumValid

let validParts:int = allParts |> Seq.mapi (fun i partsLine -> getValidParts i partsLine)  |> Seq.sum

printfn "Part One Total %d" validParts

// Part Two

let rxStar: Regex = Regex(@"\*{1}", RegexOptions.Compiled)

let allStars:List<MatchCollection> = Seq.toList (inputLines |> Seq.map(fun dataLine -> parseSchematic dataLine rxStar))

let searchAroundStars i (starPos:int) = 
    let thisLevelGears = allParts |> Seq.item i |> Seq.map(fun (part:Match) ->
        
        if (part.Index + part.Length) - starPos < (part.Length + 2) && (part.Index + part.Length) - starPos >=0 then              
            int(part.Value)
        else
            0) 
    thisLevelGears |> Seq.filter(fun x -> x > 0) 

let getGears i starLine:int = 
    starLine |> Seq.map(fun (starPos:Match) ->
        let adjList = Seq.append (Seq.append (searchAroundStars i starPos.Index) (searchAroundStars (i+1) starPos.Index)) (searchAroundStars (i-1) starPos.Index)
        let filtList = adjList |> Seq.filter(fun x -> x > 0)
        //printfn "%A" filtList
        if filtList |> Seq.length = 2 then (filtList |> Seq.item 0) * (filtList |> Seq.item 1) else 0
    ) |> Seq.sum

let starGearRatios = allStars |> List.mapi (fun i starLine -> getGears i starLine) |> List.sum

starGearRatios |> printfn "Part Two Total %d" 