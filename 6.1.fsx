open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("6.txt")

let parseInput (input: string list) =
    let getNumbers (input: string) =
        let matchCollection = Regex.Matches(input, @"(\d+)+")

        matchCollection
        |> Seq.map (fun m -> if m.Success then (List.tail [ for g in m.Groups -> g.Value |> int ]) else [])
        |> Seq.concat

    let times = getNumbers input.[0]
    let recordDistances = getNumbers input.[1]

    Seq.zip times recordDistances

let getDistanceTraveled (holdTime: int) (totalTime: int) = 
    holdTime * (totalTime - holdTime)

let canBeatRace (holdTime: int) (totalTime: int) (recordDistance: int) =
    getDistanceTraveled holdTime totalTime > recordDistance

let getNumberOfWaysToBeatRace (totalTime: int, recordDistance: int) =
    (0, seq { 1 .. totalTime })
    ||> Seq.fold 
        (fun acc curr ->
            match canBeatRace curr totalTime recordDistance with
            | true -> acc + 1
            | false -> acc)

let solve (input: string list) =
    let timesAndRecordDistances = parseInput input |> log "timesAndRecords: "

    (1, timesAndRecordDistances)
    ||> Seq.fold 
        (fun acc curr -> acc * (getNumberOfWaysToBeatRace curr))


let testData =
    [
        "Time:      7  15   30"
        "Distance:  9  40  200"
    ]

// let answer = testData |> solve


let answer = readInput () |> Seq.toList |> solve

printfn "Answer is: %A" answer


