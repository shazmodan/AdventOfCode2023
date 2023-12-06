open System.Text.RegularExpressions

let readInput () = System.IO.File.ReadLines("3.txt")

type Pos = { X: int; Y: int; Symbol: string }

type Direction =
    | Left
    | Right

let getNumbersAdjacentToGearSymbol (schematic: string array) (gearX: int) (gearY: int) =
    let rowMax = schematic[0].Length - 1
    let columnMax = schematic.Length - 1

    let isWithinXBounds (index: int) = not (index < 0) && not (index > rowMax)
    let isWithinYBounds (index: int) = not (index < 0) && not (index > columnMax)
    let isNumber (c: char) = System.Char.IsAsciiDigit c

    let rec parseNumberDirection (x: int) (y: int) (direction: Direction) (acc: char list) =
        if isWithinXBounds x && isWithinYBounds y then
            let value = schematic[y][x]
            let isDigit = System.Char.IsAsciiDigit value
            
            match isDigit with
            | true -> 
                let nextX = 
                    match direction with
                    | Direction.Left -> x - 1
                    | Direction.Right -> x + 1
                
                let newAcc = value :: acc
                
                parseNumberDirection nextX y direction newAcc
            | false -> // terminal condition
                match direction with
                | Direction.Left -> acc
                | Direction.Right -> acc |> List.rev
        else // terminal condition
            match direction with
            | Direction.Left -> acc
            | Direction.Right -> acc |> List.rev

    let parseLeft (x: int) (y: int) =  parseNumberDirection x y Direction.Left []
    let parseRight (x: int) (y: int) = parseNumberDirection x y Direction.Right []

    let leftCell = 
        let x = max (gearX - 1) 0
        parseLeft x gearY

    let rightCell = 
        let x = min (gearX + 1) (schematic[0].Length - 1)
        parseRight x gearY

    let topCells = 
        match gearY - 1 with
        | y when isWithinYBounds y ->
            let rightAbove = schematic[y][gearX]

            match isNumber rightAbove with
            | true ->
                let parsedLeft = parseLeft (gearX-1) y //parseNumberDirection (gearX-1) y Direction.Left []
                let parsedRight = parseRight (gearX+1) y //parseNumberDirection (gearX+1) y Direction.Right []
                
                let completeNumberChar = parsedLeft @ [rightAbove] @ parsedRight
                let number = System.String.Join("", completeNumberChar) |> int

                [ number ]
            | false ->
                let parsedAboveLeft = parseLeft (gearX-1) (gearY-1) 
                let parsedAboveRight = parseRight (gearX+1) (gearY+1)
                
                let numberAboveLeft = System.String.Join("", parsedAboveLeft) |> int
                let numberAboveRight = System.String.Join("", parsedAboveRight) |> int

                [ numberAboveLeft; numberAboveRight ]
        | _ -> []

    //TODO: Just do like topCells do 
    let bottomCells =
        match gearY + 1 with
        | y when isWithinYBounds y ->
            [ gearMinX-1..gearMaxX+1 ]
            |> List.filter isWithinXBounds
            |> List.exists(fun x -> isNumber (schematic[y][x]))
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

let result = testData |> solve

result
result = 467835

// readInput ()
// |> Seq.toArray
// |> solve
