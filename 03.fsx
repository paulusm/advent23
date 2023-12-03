open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("03.txt")

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
            let symbolAdjacent:int = checkAdjacency part i + (if i > 0 then checkAdjacency part (i-1) else 0) + (if i <= 138  then checkAdjacency part (i+1) else 0)
            match symbolAdjacent  with
            | 0 -> 0
            | _ -> int part.Value) |> Seq.sum
    sumValid

let validParts:int = allParts |> Seq.mapi (fun i partsLine -> getValidParts i partsLine)  |> Seq.sum

printfn "Total %d" validParts