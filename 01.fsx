open System.Text.RegularExpressions
open System

let readLines = System.IO.File.ReadLines("01.txt");;

printfn "%A"  readLines

let testLine = "b7fgghh"

let rx = Regex(@"\d|one|two|three|four|five|six|seven|eight|nine", RegexOptions.Compiled)

let matchVal theMatch = 
    match theMatch with
        | theMatch when String.length theMatch = 1 -> theMatch
        | _ -> (List.findIndex (fun (x,_) -> x = theMatch) ["one";"two";"three";"four";"five";"six";"seven";"eight";"nine"]) + 1

let calVal theLine = 
    let m = rx.Matches(theLine)
    let outSum = int ((matchVal (m.Item(0).Value)) + (matchVal (m.Item(m.Count-1).Value)))
    outSum

//printfn "%d" (calVal testLine)

let partTwo = Seq.map (fun itLine -> calVal itLine) readLines |> Seq.sum
printfn "Tot %d" partOne
