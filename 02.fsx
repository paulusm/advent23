// Limits only 12 red cubes, 13 green cubes, and 14 blue cubes

open System.Text.RegularExpressions

let inputLines = System.IO.File.ReadLines("02.txt");;

printfn "%A"  inputLines

let rxGame: Regex = Regex(@"Game (\d*):(.*)", RegexOptions.Compiled)
let rxDrawInstances: Regex = Regex(@"(\d*) (red|green|blue)", RegexOptions.Compiled)

// Part One

let checkValid (gameDraws:string[]):bool=
    // Here we go through the draws in the game
    let drawCheck = (Array.map(fun aDraw ->
        let m2: MatchCollection = rxDrawInstances.Matches(aDraw)
        // Here we go through the ball results in the draw
        let ballCheck = (Seq.map(fun (ballMatch:Match) ->
            match ballMatch.Groups.[2].Value with
                | "red" -> if int ballMatch.Groups.[1].Value > 12 then false else true
                | "green" -> if int ballMatch.Groups.[1].Value > 13 then false else true
                | "blue" -> if int ballMatch.Groups.[1].Value > 14 then false else true
                | _ -> false
        ) m2)
        //printfn "%A" ballCheck
        Seq.reduce(fun acc x -> x && acc) ballCheck
    ) gameDraws)
    Seq.reduce(fun acc x -> x && acc) drawCheck
   

let parseGames game:int = 
    let m: Match = rxGame.Match(game)
    let gameID:int = int m.Groups.[1].Value
    let draws: string[] = m.Groups.[2].Value.Split(";")
    let getValid:bool = checkValid draws
    match getValid with
        | true -> gameID
        | false -> 0

let result1 = Seq.map(fun gameLine -> parseGames gameLine) inputLines |> Seq.sum
printfn "Sum Part1 = %d" result1

// Part Two

let getBallRequirements (gameDraws:string[]):int=
     
    let ballNeeds = Map ["r", 0; "g", 0; "b", 0]

    // Here we go through the draws in the game
    let drawSums = (Array.map(fun aDraw ->
        let m2: MatchCollection = rxDrawInstances.Matches(aDraw)
        // Here we go through the ball results in the draw
        let ballCounts = (Seq.map(fun (ballMatch:Match) ->
        
            match ballMatch.Groups.[2].Value with
                | "red" -> 
                    let ballNeeds = ballNeeds |> Map.add "r" (int ballMatch.Groups.[1].Value)
                    ballNeeds
                | "green" -> 
                    let ballNeeds = ballNeeds |> Map.add "g" (int ballMatch.Groups.[1].Value)
                    ballNeeds
                | "blue" -> 
                    let ballNeeds = ballNeeds |> Map.add "b" (int ballMatch.Groups.[1].Value)
                    ballNeeds
                | _ -> ballNeeds
        ) m2)
        
        Seq.reduce(fun (acc:Map<string, int>) x -> 
          acc |> 
            Map.add "r" (x.["r"] + acc.["r"]) |> 
            Map.add "g" (x.["g"] + acc.["g"]) |>
            Map.add "b" (x.["b"] + acc.["b"])
        ) ballCounts
        
    ) gameDraws )

    let finalCounts = 
        Seq.reduce(fun (acc:Map<string, int>) x ->
                let up_r = if x.["r"] > acc.["r"] then Map.add "r" x.["r"] acc else acc
                let up_g = if x.["g"] > acc.["g"] then Map.add "g" x.["g"] up_r else up_r
                let up_b = if x.["b"] > acc.["b"] then Map.add "b" x.["b"] up_g else up_g
                up_b
        ) drawSums
    //printfn "%A" finalCounts
    finalCounts.["r"] * finalCounts.["g"] * finalCounts.["b"]
    

let parseGames2 game:int = 
    let m: Match = rxGame.Match(game)
    let gameID:int = int m.Groups.[1].Value
    let draws: string[] = m.Groups.[2].Value.Split(";")
    let getBallTots:int = getBallRequirements draws
    getBallTots

let result2 = Seq.map(fun gameLine -> parseGames2 gameLine) inputLines |> Seq.sum
printfn "Sum Part2 = %d" result2
