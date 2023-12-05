open System.Text.RegularExpressions

let input = System.IO.File.ReadLines("../data/05-test.txt")

// the destination range start, the source range start, and the range length.

let rxPart: Regex = Regex(@"^$", RegexOptions.Compiled)
let rxData: Regex = Regex(@"\d+", RegexOptions.Compiled)

let parsedInput = input |> Seq.map(fun x ->
     rxData.Matches(x) 
)

printfn "%A parts" parsedInput

