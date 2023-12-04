open System.Text.RegularExpressions

let readInput () = System.IO.File.ReadLines("2.txt")

type CubeSet = { Blue: int; Red: int; Green: int }

type Line = { Game: int; CubeSets: CubeSet array }

let strToLine (str: string) : Line =
    let gameId = Regex.Match(str, "Game (\d+):").Groups.[1].Value |> int

    let afterColon = str.Substring(str.IndexOf(":"))
    let cubeSets = afterColon.Split(";")

    let parseCubeSet (cubeSet: string) : CubeSet =
        let colorMatch (color: string) : int =
            let regexMatch = Regex.Match(cubeSet, sprintf "(\d+) %s" color)

            if regexMatch.Success then
                regexMatch.Groups[1].Value |> int
            else
                0

        let red = colorMatch "red"
        let green = colorMatch "green"
        let blue = colorMatch "blue"

        { Blue = blue
          Red = red
          Green = green }

    { Game = gameId
      CubeSets = cubeSets |> Array.map parseCubeSet }


let idSumOfValidGames (maxRed: int) (maxGreen: int) (maxBlue: int) (lines: Line seq) =
    lines
    |> Seq.filter (fun line ->
        line.CubeSets
        |> Array.forall (fun cubeSet ->
            let apa =
                cubeSet.Red <= maxRed && cubeSet.Green <= maxGreen && cubeSet.Blue <= maxBlue

            printfn "%A" cubeSet
            printfn "%b" apa
            apa))
    |> Seq.sumBy (fun line -> line.Game)


// let testData =
//     [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
//       "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
//       "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
//       "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
//       "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]

// testData |> List.map strToLine |> idSumOfValidGames 12 13 14

readInput () |> Seq.map strToLine |> idSumOfValidGames 12 13 14
