open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("10.txt")

let inputToArray2D (input: string seq) =
    input
    |> Seq.map (fun s -> s.ToCharArray())
    |> array2D

type Pos = { X: int; Y: int }

type Direction =
    | North
    | South
    | East
    | West

let getDirection (startPos: Pos) (nextPos: Pos) : Direction =
    match (nextPos.X - startPos.X), (nextPos.Y - startPos.Y) with
    | (0, 1) -> South
    | (0, -1) -> North
    | (1, 0) -> East
    | (-1, 0) -> West
    | x -> failwithf "Invalid direction: %A" x

let isValidNextPos (tiles: char array2d) (currentPos: Pos) (nextPos: Pos) : bool =
    let direction = getDirection currentPos nextPos
    let nextChar = tiles.[nextPos.Y, nextPos.X] 

    let northConnectors = [ '|';'F'; '7' ]
    let southConnectors = [ '|';'L'; 'J' ]
    let eastConnectors = [ '-'; 'J'; '7' ]
    let westConnector = [ '-'; 'L'; 'F' ]
    
    // | is a vertical pipe connecting north and south.
    // - is a horizontal pipe connecting east and west.
    // L is a 90-degree bend connecting north and east.
    // J is a 90-degree bend connecting north and west.
    // 7 is a 90-degree bend connecting south and west.
    // F is a 90-degree bend connecting south and east.
    // . is ground; there is no pipe in this tile.
    // S is the starting position of the animal; 
    //       there is a pipe on this tile, but your sketch 
    //       doesn't show what shape the pipe has.

    let validChars =
        match tiles[currentPos.Y, currentPos.X] with
        | '|' ->
            match direction with
            | North -> northConnectors
            | South -> southConnectors
            | East -> []
            | West -> []
        | '-' ->
            match direction with
            | North -> []
            | South -> []
            | East -> eastConnectors
            | West -> westConnector
        | 'L' ->
            match direction with
            | North -> northConnectors
            | South -> []
            | East -> eastConnectors
            | West -> []
        | 'J' ->
            match direction with
            | North -> northConnectors
            | South -> []
            | East -> []
            | West -> westConnector
        | '7' ->
            match direction with
            | North -> []
            | South -> southConnectors
            | East -> []
            | West -> westConnector
        | 'F' ->
            match direction with
            | North -> []
            | South -> southConnectors
            | East -> eastConnectors
            | West -> []
        | '.' -> []
        | 'S' ->
            match direction with
            | North -> northConnectors
            | South -> southConnectors
            | East -> eastConnectors
            | West -> westConnector
        | x -> failwithf "[isValidNextPos] Unexpected char: '%c'" x

    validChars |> List.contains nextChar

let getStartPos (tiles: char array2d) : Pos =
    let mutable pos = { X = 0; Y = 0 }

    tiles
    |> Array2D.iteri 
        (fun y x v -> 
            if v = 'S' then 
                pos <- { X = x; Y = y } 
            else
                ())

    pos
 
let solve (input: string seq) = 
    let tiles =
        input
        |> inputToArray2D
    
    printfn "%A" tiles



let testData1 =
    [
        "....."
        ".S-7."
        ".|.|."
        ".L-J."
        "....."
    ]

let testData2 =
    [
        "-L|F7"
        "7S-7|"
        "L|7||"
        "-L-J|"
        "L|-JF"
    ]

let testData3 =
    [
        "..F7."
        ".FJ|."
        "SJ.L7"
        "|F--J"
        "LJ..."
    ]

let testData4 =
    [
        "7-F7-"
        ".FJ|7"
        "SJLL7"
        "|F--J"
        "LJ.LJ"
    ]


let answer = testData1 |> solve
//let answer = readInput () |> solve

printfn "Answer: %A" answer