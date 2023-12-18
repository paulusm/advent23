open System
//161746 too high
let inputData = System.IO.File.ReadLines("../data/14.txt") 

let dishArray = inputData |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) |> array2D 
//Part One
let calcWeight = 
    [|0..Array2D.length1 dishArray-1|] |> Array.map(fun a ->
        //printfn "%d" a
        let dishColumn =  dishArray.[*,a]
        let dishString = dishColumn |> System.String 
        let sortedString = dishString.Split '#' |> Seq.map(fun b ->
            b |> Seq.toArray |> Array.sortDescending |> System.String
        ) 
        sortedString |> String.concat "#" |> Seq.toArray 
     |> 
        Array.mapi(fun f g ->
                if g = 'O' then
                    (Array2D.length1 dishArray) - f
                else
                    0
        ) |> Array.sum
    ) |> Array.sum

printfn "%A" calcWeight