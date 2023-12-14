open System.Text.RegularExpressions
open System


let patternsRaw:string = System.IO.File.ReadAllText("../data/13-test.txt")

let patternsArray =  patternsRaw.Split "\n\n"

let foldPositions (thePattern:string)=
    // turn into a 2D array of chars
    let patternMap = thePattern.Split "\n" |> Array.filter(fun x->not(x="")) |> Array.map(fun y -> Seq.toArray y) |> array2D
    let (dimx, dimy) = (Array.length patternMap.[*,0], Array.length patternMap.[0,*])
    let horizontalMirror = [0..dimx-2] |> List.map(fun i-> 
       // Check for horizontal mirroring
       if patternMap.[i,*]  = patternMap.[i+1,*] then
            [0..dimx-i] |> List.map( fun j-> 
                if i-j <0 || i+j > dimx then
                    true
                else
                    if patternMap.[i-j,*]  = patternMap.[i+j+1,*] then true else false) |> List.reduce(fun acc z->acc && z)
        else false
    )
    let verticalMirror = [0..dimy-2] |> List.map(fun i-> 
       // Check for horizontal mirroring
       if patternMap.[*,i]  = patternMap.[*,i+1] then
            [0..dimx-i] |> List.map( fun j-> 
                if i-j <0 || i+j > dimx then
                   true
                else
                    if patternMap.[*,i-j]  = patternMap.[*,i+j+1] then true else false) |> List.reduce(fun acc z->acc && z)
        else false
    )
    (verticalMirror, horizontalMirror)

let findFolds (theString:string) =
    "OK"

let processPatterns = patternsArray |> Array.map(fun x->
    findFolds x
)

printfn "%A" processPatterns