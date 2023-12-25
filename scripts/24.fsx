open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("../data/24.txt")

let rxHail: Regex = Regex(@"-?\d+", RegexOptions.Compiled)

// This function from Stack Overflow https://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }

let trajectories = inputLines |> Seq.map(fun x->
    let hailMatches = rxHail.Matches x
    hailMatches
) 

let trajCombinations = (combinations [] 2 (Seq.toList trajectories)) |> Seq.map(fun x->
    let xa:float = float (x |> Seq.item 0 |> Seq.item 0).Value
    let ya:float = float (x |> Seq.item 0 |> Seq.item 1).Value
    let xb:float = float (x |> Seq.item 1 |> Seq.item 0).Value
    let yb:float = float (x |> Seq.item 1 |> Seq.item 1).Value
    let vxa:float = float (x |> Seq.item 0 |> Seq.item 3).Value
    let vya:float = float (x |> Seq.item 0 |> Seq.item 4).Value
    let vxb:float = float (x |> Seq.item 1 |> Seq.item 3).Value
    let vyb:float = float (x |> Seq.item 1 |> Seq.item 4).Value
    let ma:float =  vya / vxa
    let mb:float =  vyb / vxb
    let xa2:float = xa + vxa
    let ya2:float = ya + vya
    let xb2:float = xb + vxb
    let yb2:float = yb + vyb
    let ca:float = ya - ma * xa
    let cb:float = yb - mb * xb
    let a:float = (cb - ca) / (ma - mb)
    let b:float = ma * a + ca
    //[xa;ya;xb;yb;a;b;ma;mb;(if a >= 7 && a <= 27 && b >=7 && b <= 27 then 1 else 0)]

    if ((xa-xb)**2 + (ya-yb) **2 > (xa2-xb2)**2 + (ya2-yb2)**2) && (a >= 200000000000000.0 && a <= 400000000000000.0 && b >= 200000000000000.0 && b <= 400000000000000.0) then
        1
    else
        0
 )

//12584 too low, 22071 too high
trajCombinations |>  Seq.sum |> printfn "%A" //