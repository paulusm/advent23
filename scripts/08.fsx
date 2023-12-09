open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/08-test.txt") 

let rxInstructions: Regex = Regex(@"[RL]", RegexOptions.Compiled)
let rxSteps: Regex = Regex(@"[A-Z]{3}", RegexOptions.Compiled)

let instructions:MatchCollection = rxInstructions.Matches (Seq.item 0 input)

let theStepMap:Map<string,list<string>> = 
    let stepMap = Map.empty
    let stepSeq = input |> Seq.skip 2 |> Seq.map(fun x -> 
        rxSteps.Matches x
    )
    let stepMap = stepSeq |> Seq.map(fun x -> (x.[0].Value, [x.[1].Value; x.[2].Value])) |> Map.ofSeq
    stepMap

let rec stepCount (lastStep:string) (accSteps:int):int = 
    let keyMatch:list<string> = theStepMap |> Map.find lastStep
    let theSide:string = instructions.[0].Value
    let nextStep = keyMatch.[if theSide = "L" then 0 else 1]
    let iterSteps = instructions |> Seq.mapi(fun i x-> 
        let nextStep = keyMatch.[if theSide = "L" then 0 else 1]
        let keyMatch:list<string> = theStepMap |> Map.find nextStep
        nextStep) 
    let takenSteps = iterSteps |> Seq.takeWhile (fun s -> not (s = "ZZZ")) 
    let instructionCount = Seq.length instructions
    let lastStep:string = Seq.last takenSteps
    printfn "%s" lastStep
    let interimStepCount:int = Seq.length takenSteps
    printfn "%d %d" interimStepCount instructionCount
    if interimStepCount < instructionCount then
         interimStepCount + accSteps
    else
         interimStepCount + accSteps //stepCount lastStep accSteps + interimStepCount
    
printfn "Part One Result %A" (stepCount "AAA" 0)
