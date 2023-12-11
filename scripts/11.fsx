

let theImageRaw = System.IO.File.ReadLines("./data/11-test.txt") |> Seq.toArray |> Array.map(fun x-> Seq.toArray x) 
let theImage = array2D theImageRaw

let theImageDim = Array.length theImage.[0,*]-1

let rowHasGalaxy = [|0..theImageDim|] |> Array.map(fun x -> theImage.[x,*] |> Array.exists(fun x -> x = '#'))
let colHasGalaxy = [|0..theImageDim|] |>  Array.map(fun x -> theImage.[*,x] |> Array.exists(fun x -> x = '#'))


let expandBlanks inImage = (rowHasGalaxy |> Array.mapi(fun i x->
    match x with
        | true -> inImage //|> Array.insertManyAt i inImage [|'t';'t';'t';|]
        | false -> inImage
    )
)


printfn "%A" (expandBlanks theImage)