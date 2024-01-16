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

let getDirection (startPos: Pos) (nextPos: Pos) : Direction option =
    match (nextPos.X - startPos.X), (nextPos.Y - startPos.Y) with
    | (0, 1) -> Some South
    | (0, -1) -> Some North
    | (1, 0) -> Some East
    | (-1, 0) -> Some West
    | x -> None

let isValidNextPos (tiles: char array2d) (currentPos: Pos) (nextPos: Pos) : bool =
    match getDirection currentPos nextPos with
    | None -> false
    | Some direction ->
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

let isWithinBounds (tiles: char array2d) (pos: Pos) =
    pos.X >= 0 && pos.Y >= 0 && pos.Y <= tiles.GetUpperBound 0 && pos.X <= tiles.GetUpperBound 1

let getProximalTiles (tiles: char array2d) (currentPos: Pos) =
    let north = { X = currentPos.X; Y = currentPos.Y - 1 }
    let east = { X = currentPos.X + 1; Y = currentPos.Y }
    let south = { X = currentPos.X; Y = currentPos.Y + 1 }
    let west = { X = currentPos.X - 1; Y = currentPos.Y }

    [ north; east; south; west ]
    |> List.filter (isWithinBounds tiles)

let getVisited (tiles: char array2d) (startPos: Pos) : Set<Pos> =
    let getNextPos (pos: Pos) (visited: Set<Pos>) =
        getProximalTiles tiles pos
        |> List.filter 
            (fun proxPos -> 
                isValidNextPos tiles pos proxPos 
                && not (visited.Contains proxPos))

    let rec walk (posA: Pos) (posB: Pos) (visited: Set<Pos>) (count: int) =
        // printfn "posA X=%i Y=%i '%c'" posA.X posA.Y tiles[posA.Y, posA.X]
        // printfn "posB X=%i Y=%i '%c'" posB.X posB.Y tiles[posB.Y, posB.X]
        match visited.Contains posA || visited.Contains posB with 
        | true -> visited.Add posA
        | false ->
            let aProx = getNextPos posA visited
            let bProx = getNextPos posB visited
            let newVisited = (visited.Add posA).Add posB

            match aProx |> List.tryHead, bProx |> List.tryHead with
            | None, _
            | _, None -> visited.Add posA
            | Some newPosA, Some newPosB -> walk newPosA newPosB newVisited (count + 1)

    let startProx = 
        startPos 
        |> getProximalTiles tiles 
        |> List.filter (isValidNextPos tiles startPos)

    match startProx with
    | [ startPosA; startPosB ] -> walk startPosA startPosB ([startPos] |> Set) 1
    | x -> failwithf "Unexpected startPos: %A" x

let getTilesWithinLoop (tiles: char array2d) (visited: Set<Pos>) =
    let rec walkTheRow (currentPos: Pos) (isWithinVisited: bool) (count: int) : int =
        printfn "Y: %i X: %i, isWithinVisited: %b, count: %i" currentPos.Y currentPos.X isWithinVisited count
        if Array2D.length2 tiles <= currentPos.X then
            // Terminal conditon for row -> go to next row.
            let nextPos = { currentPos with X = 0; Y = currentPos.Y + 1 }
            walkTheRow nextPos false count
        elif Array2D.length1 tiles <= currentPos.Y then
            count // Terminal condition for column -> we are done.
        else
            let nextPos = { currentPos with X = currentPos.X + 1 }
            
            match isWithinVisited, Set.contains currentPos visited  with
            | true, true -> walkTheRow nextPos true count
            | true, false -> walkTheRow nextPos true (count+1)
            | false, false  -> walkTheRow nextPos false count
            | false, true -> walkTheRow nextPos true count

    walkTheRow { X = 0; Y = 0} false 0


let solve (input: string seq) = 
    let tiles =
        input
        |> inputToArray2D
    
    let startPos = getStartPos tiles

    let visited = getVisited tiles startPos

    Set.toArray visited |> Array.sortBy (fun pos -> pos.Y, pos.X) |> printfn "%A" |> ignore

    getTilesWithinLoop tiles visited

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
        "..........."
        ".S-------7."
        ".|F-----7|."
        ".||.....||."
        ".||.....||."
        ".|L-7.F-J|."
        ".|..|.|..|."
        ".L--J.L--J."
        "..........."
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