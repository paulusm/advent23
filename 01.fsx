open System.Text.RegularExpressions
open System

let readLines = System.IO.File.ReadLines("01.txt");;

printfn "%A"  readLines

let testLine = "eightwothree"

// Part one is just @"\d"
let rx = Regex(@"\d|one|two|three|four|five|six|seven|eight|nine", RegexOptions.Compiled)

let matchVal theMatch = 
    match theMatch with
        | theMatch when String.length theMatch = 1 -> theMatch
        | _ -> string (List.findIndex (fun (x) -> x = theMatch) ["one";"two";"three";"four";"five";"six";"seven";"eight";"nine"] + 1) 

let calVal theLine = 
    let m = rx.Matches(theLine)
    //nb the inner + is to concatanate the two digit strings
    let outSum = int ((matchVal (m.Item(0).Value)) + (matchVal (m.Item(m.Count-1).Value)))
    outSum

printfn "%d" (calVal testLine)

let resultSum = Seq.map (fun itLine -> calVal itLine) readLines |> Seq.sum
printfn "Tot %d" resultSum
