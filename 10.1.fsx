open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("10.txt")


let solve (input: string seq) = ()

let testData1 =
    [

    ]


let answer = testData1 |> solve
//let answer = readInput () |> solve

printfn "Answer: %A" answer