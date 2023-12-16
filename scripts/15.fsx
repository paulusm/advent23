let inputData = System.IO.File.ReadAllText("../data/15.txt") 

let steps = inputData.Split(',')

let stepValue cumTotal theStep = 
    theStep |> Seq.mapi (fun i theChar -> 
            if i=0 then int theChar * 17 % 256 else int theChar 
    ) |> Seq.reduce(fun acc charVal-> ((acc + charVal) * 17) % 256)

let result = steps |> Array.map(fun step ->
    stepValue 0 step
) 

printfn "%A" (result |> Array.sum)