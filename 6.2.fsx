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

        let bigNumber =
            matchCollection
            |> Seq.map (fun m -> if m.Success then (List.tail [ for g in m.Groups -> g.Value ]) else [])
            |> Seq.concat
            |> Seq.toArray
        
        System.String.Join("", bigNumber)
        |> int64

    let times = getNumbers input.[0]
    let recordDistances = getNumbers input.[1]

    seq {(times, recordDistances)}

let getDistanceTraveled (holdTime: int64) (totalTime: int64) = 
    holdTime * (totalTime - holdTime)

let canBeatRace (holdTime: int64) (totalTime: int64) (recordDistance: int64) =
    getDistanceTraveled holdTime totalTime > recordDistance

let getNumberOfWaysToBeatRace (totalTime: int64, recordDistance: int64) =
    (0L, seq { 1L .. totalTime })
    ||> Seq.fold 
        (fun acc curr ->
            match canBeatRace curr totalTime recordDistance with
            | true -> acc + 1L
            | false -> acc)

let solve (input: string list) =
    let timesAndRecordDistances = parseInput input

    (1L, timesAndRecordDistances)
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


