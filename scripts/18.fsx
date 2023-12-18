open System.Text.RegularExpressions

let planRaw = System.IO.File.ReadLines("../data/18-test.txt")
let rxData: Regex = Regex(@"([RDLU]) (\d+) \((#[0-9a-z]{6})\)", RegexOptions.Compiled)

let lavaPlan = Array2D.init 100 100 (fun x y->0)

let initPos = (0,0)

let parsedPlan = 
    planRaw |> Seq.map(fun x->
        let planElements = x |> rxData.Match 
        let digDirection = planElements.Groups.[1].Value 
        let digDistance = int(planElements.Groups.[2].Value)
        let digColour =  planElements.Groups.[3].Value
        let newPos = match digDirection with
            | "R" -> (digDistance, 0)
            | "L" -> ( - digDistance,0)
            | "D" -> (0,  + digDistance)
            | "U" -> (0,  - digDistance)
            | _ -> (0,0)
        let currentX, currentY = newPos
        (newPos, digColour)
    )

printfn "%A" parsedPlan