open System.Text.RegularExpressions
open System


let patternsRaw:string = System.IO.File.ReadAllText("../data/13.txt")

let patternsArray =  patternsRaw.Split "\n\n"

let foldPositions (thePattern:string)=
    // turn into a 2D array of chars
    let patternMap = thePattern.Split "\n" |> Array.filter(fun x->not(x="")) |> Array.map(fun y -> Seq.toArray y) |> array2D
    let (dimx, dimy) = (Array.length patternMap.[*,0], Array.length patternMap.[0,*])
    let horizontalMirror = [0..dimx-2] |> List.map(fun i-> 
       // Check for vertical mirroring
       if patternMap.[i,*]  = patternMap.[i+1,*] then
            [0..dimx-i] |> List.map( fun j-> 
                if i-j <0 || i+j+1 >= dimx then
                    true
                else
                    if patternMap.[i-j,*]  = patternMap.[i+j+1,*] then true else false) |> List.reduce(fun acc z->acc&&z)
        else false
    )
    let verticalMirror = [0..dimy-2] |> List.map(fun i-> 
       // Check for horizontal mirroring
       if patternMap.[*,i]  = patternMap.[*,i+1] then
            [0..dimy-i] |> List.map( fun j-> 
                if i-j <0 || i+j+1 >= dimy then
                 true
                else
                    //printfn "parms %d %d %d" i j dimy
                    if patternMap.[*,i-j]  = patternMap.[*,i+j+1] then true else false) |> List.reduce(fun acc z->acc&&z)
        else false
    )
    let foldPosV = verticalMirror |> List.mapi(fun i x-> if x then i+1 else 0) |> List.reduce(fun acc x->if x>acc then x else acc)
    let foldPosH = horizontalMirror |> List.mapi(fun i x-> if x then i+1 else 0) |> List.reduce(fun acc x->if x>acc then x else acc)
    (foldPosV, foldPosH)

let findFolds (theString:string) =
    let(vert, hor)  = foldPositions theString
    //printfn "%A %A" vert hor
    (vert, hor)

let processPatterns = patternsArray |> Array.map(fun x->
    //printfn "pattern %A" x
    let (v,h) = findFolds x
    v + 100*h
)

printfn "%A" (processPatterns |> Array.sum)