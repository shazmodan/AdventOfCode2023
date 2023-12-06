open System.Text.RegularExpressions

let readInput () = System.IO.File.ReadLines("3.txt")

type Pos = { X: int; Y: int; Symbol: string }

let isAdjacentToSymbol (schematic: string array) (numberMinX: int) (numberMaxX: int) (numberY: int) =
    let rowMax = schematic[0].Length - 1
    let columnMax = schematic.Length - 1

    let isSymbol (c: char) = 
        let a = not (System.Char.IsAsciiDigit c) && c <> '.'
        // printfn "%A" {| Char = c; IsSymbol = a |} //TODO:REMOVE
        a

    let isWithinXBounds (index: int) = not (index < 0) && not (index > rowMax)
    let isWithinYBounds (index: int) = not (index < 0) && not (index > columnMax)

    let leftCell = 
        let x = max (numberMinX - 1) 0
        isSymbol (schematic[numberY][x])
    
    let rightCell = 
        let x = min (numberMaxX + 1) (schematic[0].Length - 1)
        isSymbol (schematic[numberY][x])

    let topCells = 
        match numberY - 1 with
        | y when isWithinYBounds y ->
            [ numberMinX-1..numberMaxX+1 ]
            |> List.filter isWithinXBounds
            |> List.exists(fun x -> isSymbol (schematic[y][x]))
        | _ -> false

    let bottomCells =
        match numberY + 1 with
        | y when isWithinYBounds y ->
            [ numberMinX-1..numberMaxX+1 ]
            |> List.filter isWithinXBounds
            |> List.exists(fun x -> isSymbol (schematic[y][x]))
        | _ -> false

    [leftCell; rightCell; topCells; bottomCells] |> List.exists id


let solve (schematic: string array) =
    schematic
    |> Array.mapi 
        (fun y row ->
            let regexMatch = Regex.Matches(row, "(\d+)")

            regexMatch
            |> Seq.sumBy 
                (fun curr ->
                    match curr.Success with
                    | true ->
                        let minX = curr.Groups.[1].Index
                        let maxX = minX + curr.Groups.[1].Length - 1

                        // printfn "%A" {| Number = (curr.Groups.[1].Value |> int); MinX = minX; MaxX = maxX; Y = y |} //TODO: REMOVE

                        match isAdjacentToSymbol schematic minX maxX y with
                        | true -> (curr.Groups.[1].Value |> int)
                        | false -> 0
                    | false -> 0
                )
        )
    |> Array.sum


let testData = [|
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
|]

//testData |> solve

readInput ()
|> Seq.toArray
|> solve
