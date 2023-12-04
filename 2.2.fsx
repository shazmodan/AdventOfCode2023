open System.Text.RegularExpressions

let readInput () = System.IO.File.ReadLines("2.txt")

type CubeSet = { Blue: int option; Red: int option; Green: int option }

type Line = { Game: int; CubeSets: CubeSet array }

let strToLine (str: string) : Line =
    let gameId = Regex.Match(str, "Game (\d+):").Groups.[1].Value |> int

    let afterColon = str.Substring(str.IndexOf(":"))
    let cubeSets = afterColon.Split(";")

    let parseCubeSet (cubeSet: string) : CubeSet =
        let colorMatch (color: string) : int option =
            let regexMatch = Regex.Match(cubeSet, sprintf "(\d+) %s" color)

            if regexMatch.Success then
                regexMatch.Groups[1].Value |> int |> Some
            else
                None

        let red = colorMatch "red"
        let green = colorMatch "green"
        let blue = colorMatch "blue"

        { Blue = blue
          Red = red
          Green = green }

    { Game = gameId
      CubeSets = cubeSets |> Array.map parseCubeSet }


let testData =
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]

let fewestNrOfCubesEachColor (line: Line) =
    let getMinColor (accColorOpt: int option) (currentColorOpt: int option) =
        match accColorOpt, currentColorOpt with
        | Some accColorOpt, Some currentColor -> System.Math.Max(accColorOpt, currentColor) |> Some
        | None, Some currentColor -> currentColor |> Some
        | Some accColorOpt, None -> accColorOpt |> Some
        | None, None -> None

    // (R,G,B)
    (((None: int option), (None: int option ), (None: int option)), line.CubeSets)
    ||> Array.fold 
        (fun (redOpt, greenOpt, blueOpt) current -> 
            let newRedOpt = getMinColor redOpt current.Red
            let newGreenOpt = getMinColor greenOpt current.Green
            let newBlueOpt = getMinColor blueOpt current.Blue

            (newRedOpt, newGreenOpt, newBlueOpt))
    |> (fun (redOpt, greenOpt, blueOpt) ->
        redOpt |> Option.defaultValue 0,
        greenOpt |> Option.defaultValue 0,
        blueOpt |> Option.defaultValue 0
        )

let multiply (r: int, g: int, b: int) = r * g * b

let solve () =
    readInput()
    |> Seq.sumBy (strToLine >> fewestNrOfCubesEachColor >> multiply)

solve()

// testData
// |> List.sumBy (strToLine >> fewestNrOfCubesEachColor >> multiply)
