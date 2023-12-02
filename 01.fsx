open System.Text.RegularExpressions
open System

let readLines: Collections.Generic.IEnumerable<string> = System.IO.File.ReadLines("01.txt");;

printfn "%A"  readLines

let testLine: string = "eightwo"

// Part one is just @"\d"
let rx: Regex = Regex(@"(?=(\d|one|two|three|four|five|six|seven|eight|nine))", RegexOptions.Compiled)

let matchVal (theMatch: string) = 
    match theMatch with
        | theMatch when String.length theMatch = 1 -> theMatch
        | _ -> string (List.findIndex (fun (x: string) -> x = theMatch) ["one";"two";"three";"four";"five";"six";"seven";"eight";"nine"] + 1) 

let calVal (theLine: string) = 
    let m: MatchCollection = rx.Matches(theLine)
    //nb the inner + is to concatanate the two digit strings
    let outSum: int = int ((matchVal (m.Item(0).Groups.[1].Value)) + (matchVal (m.Item(m.Count-1).Groups.[1].Value)))
    outSum

//printfn "%d" (calVal testLine)

let resultSum: int = Seq.map (fun (itLine: string) -> calVal itLine) readLines |> Seq.sum
printfn "Tot %d" resultSum
