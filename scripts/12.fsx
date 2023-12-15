
let inputData = System.IO.File.ReadLines("./data/12-test.txt") 

let result = inputData |> Seq.map(fun x ->
    let springLine = x.Split ' '
    springLine
)

printfn "%A" result