open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("7.txt")

let solve (input: string seq) = ()

let testData =
    [
        "RL"

        "AAA = (BBB, CCC)"
        "BBB = (DDD, EEE)"
        "CCC = (ZZZ, GGG)"
        "DDD = (DDD, DDD)"
        "EEE = (EEE, EEE)"
        "GGG = (GGG, GGG)"
        "ZZZ = (ZZZ, ZZZ)"
    ]

// let answer = readInput () |> solve
let answer = testData |> solve

printfn "Answer: %A" answer