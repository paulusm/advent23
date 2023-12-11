

let theImageRaw = System.IO.File.ReadLines("../data/11-test.txt") |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) 
let theImage = array2D theImageRaw

let theImageDim = Array.length theImage.[0,*]-1

let rowHasGalaxy = [|0..theImageDim|] |> Array.map(fun x -> theImage.[x,*] |> Array.exists(fun x -> x = '#'))
let colHasGalaxy = [|0..theImageDim|] |>  Array.map(fun x -> theImage.[*,x] |> Array.exists(fun x -> x = '#'))


let expandBlanks how (inImage:char[,])= (
    [|0..theImageDim|] |> Array.mapi(fun i x->
        let checkBlanks = if how="row" then rowHasGalaxy else colHasGalaxy
        match checkBlanks.[i] with
        | false when how="row" -> [|inImage.[i,*];inImage.[i,*]|]
        | true when how="row" -> [|inImage.[i,*]|]
        | false when how="col" -> [|inImage.[*,i];inImage.[*,i]|]
        | true when how="col" -> [|inImage.[*,i]|]
        | _ -> [|inImage.[i,*]|]
    )
)

let expandedRows = expandBlanks "row" theImage |> Array.map (fun x->
     x |> Array.map(fun y->y)
)
printfn "%A" (expandedRows)