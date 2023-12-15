 
open System.Text.RegularExpressions
let inputData = System.IO.File.ReadLines("../data/12-test.txt") 

let rec runMatches  (theString:string) (theNo:string) (theLastPos:int) = 
    let theTestString = theString[theLastPos..]
    printfn "%d" theLastPos
    let reTest =  Regex ("([#?]{" + theNo + "})(^#|$)")
    let hashMatch = reTest.Match theTestString
    printfn "%A" hashMatch.Groups.[1]
    if not hashMatch.Success then
        let reTest2 =  Regex ("([#?]{" + theNo + "})[.$]{1}")
        let varMatch = reTest2.Match theTestString 
        printfn "%A" varMatch.Groups.[1]
        (varMatch.Groups.[1].Index + varMatch.Groups.[1].Value.Length-1, varMatch.Groups.[1].Value)
    else
        (hashMatch.Groups.[1].Index + hashMatch.Groups.[1].Value.Length-1, hashMatch.Groups.[1].Value)

let result = inputData |> Seq.map(fun x ->
    let springLine = x.Split ' '
    let mutable startPos = 0
    springLine.[1].Split ',' |> Array.map(fun seqLen ->
        // Call for each segment
        let (newStart, theMatch) = runMatches springLine.[0] seqLen (startPos)
        startPos <- newStart
        theMatch
    )
)

printfn "%A" (result |> Seq.map(fun x-> x))
