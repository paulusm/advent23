open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadLines("../data/08.txt") 

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

let stepJump (theSide:string) (lastStep:string) (accSteps:int) = 
    let keyMatch:list<string> = theStepMap |> Map.find lastStep
    let nextStep = keyMatch.[if theSide = "L" then 0 else 1]
    printfn "Choice - %s Last - %s Next -%s" theSide lastStep nextStep
    nextStep

let rec iterSteps initInstruction initCode accSteps = 
    let startPoint = if initInstruction = (Seq.length instructions) then 0 else initInstruction
    let theSide:Match = instructions |> Seq.item startPoint
    let theNextStep = stepJump theSide.Value initCode accSteps
    if theNextStep = "ZZZ"
        then accSteps + 1
    else
        iterSteps (startPoint + 1) theNextStep accSteps + 1

printfn "Part One Result %A" (iterSteps 0 "AAA" 0)
