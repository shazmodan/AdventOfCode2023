open System.Text.RegularExpressions

let readInput () = System.IO.File.ReadLines("3.txt")

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

        let parsedLeft = 
            parseLeft x gearY
            |> Array.ofList 
            |> System.String.Concat

        match System.String.IsNullOrWhiteSpace parsedLeft with
        | true -> None
        | false -> Some (int parsedLeft)


    let rightCell = 
        let x = min (gearX + 1) (schematic[0].Length - 1)
        
        let parsedRight = 
            parseRight x gearY
            |> Array.ofList 
            |> System.String.Concat
        
        match System.String.IsNullOrWhiteSpace parsedRight with
        | true -> None
        | false -> Some (int parsedRight)

    let topCells = 
        match gearY - 1 with
        | y when isWithinYBounds y ->
            let rightAbove = schematic[y][gearX]

            match isNumber rightAbove with
            | true ->
                let parsedLeft = parseLeft (gearX-1) y
                let parsedRight = parseRight (gearX+1) y
                
                let completeNumberChar = parsedLeft @ [rightAbove] @ parsedRight
                let number = System.String.Join("", completeNumberChar)

                [ number ]
            | false ->
                let parsedAboveLeft = parseLeft (gearX-1) y
                let parsedAboveRight = parseRight (gearX+1) y
                
                let numberAboveLeft = System.String.Join("", parsedAboveLeft)
                let numberAboveRight = System.String.Join("", parsedAboveRight)

                [ numberAboveLeft; numberAboveRight ]
        | _ -> []
        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
        |> List.map int

    let bottomCells =
        match gearY + 1 with
        | y when isWithinYBounds y ->
            let rightBelow = schematic[y][gearX]

            match isNumber rightBelow with
            | true ->
                let parsedLeft = parseLeft (gearX-1) y
                let parsedRight = parseRight (gearX+1) y
                
                let completeNumberChar = parsedLeft @ [rightBelow] @ parsedRight
                let number = System.String.Join("", completeNumberChar)

                [ number ]
            | false ->
                let parsedBelowLeft = parseLeft (gearX-1) y
                let parsedBelowRight = parseRight (gearX+1) y
                
                let numberBelowLeft = System.String.Join("", parsedBelowLeft)
                let numberAboveRight = System.String.Join("", parsedBelowRight)

                [ numberBelowLeft; numberAboveRight ]
        | _ -> []
        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
        |> List.map int

    let adjacentNumbers =
        [
            if leftCell.IsSome then leftCell.Value
            if rightCell.IsSome then rightCell.Value ] @ topCells @ bottomCells

    adjacentNumbers


let solve (schematic: string array) =
    schematic
    |> Array.mapi 
        (fun y row ->
            let regexMatch = Regex.Matches(row, "(\*)")

            regexMatch
            |> Seq.sumBy 
                (fun curr ->
                    match curr.Success with
                    | true ->
                        let x = curr.Groups.[1].Index

                        let adjacentNumbers = 
                            let numbers = getNumbersAdjacentToGearSymbol schematic x y
                            
                            match numbers.Length > 1 with
                            | true -> numbers
                            | false -> []

                        let gearRatio =
                            (None, adjacentNumbers)
                            ||> List.fold 
                                (fun accOpt curr ->
                                    match accOpt with
                                    | None -> Some curr
                                    | Some acc -> Some (curr * acc))
                            |> Option.defaultValue 0

                        gearRatio
                    | false -> 0
                )
        )
    |> Array.sum


// let testData = [|
//     "467..114.."
//     "...*......"
//     "..35..633."
//     "......#..."
//     "617*......"
//     ".....+.58."
//     "..592....."
//     "......755."
//     "...$.*...."
//     ".664.598.."
// |]

// let result = testData |> solve

// result
// result = 467835

readInput ()
|> Seq.toArray
|> solve
