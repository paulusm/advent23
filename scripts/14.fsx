//161746 too high

let inputData = System.IO.File.ReadLines("../data/14-test.txt") 
open System

let dishArray = inputData |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) |> array2D 
//Part One
let calcWeight = 
    [|0..Array2D.length1 dishArray-1|] |> Array.map(fun a ->
        //printfn "%d" a
        let dishCol =  dishArray.[*,a]
        dishCol |>
        Array.sortWith(fun h i ->
            match (h,i) with
            | ('#','O') |('O','#')| ('#','.') |('.','#') -> 0
            | _ -> (compare h i) * -1
    ) ) |> Array.mapi(fun f g -> 
            printfn "%d %A" f g
    
    )
    //     ) |> 
    //     Array.mapi(fun f g ->
    //             if g = 'O' then
    //                 (Array2D.length1 dishArray) - f
    //             else
    //                 0
    //     ) |> Array.sum
    // ) |> Array.sum

//printfn "%A" 
calcWeight