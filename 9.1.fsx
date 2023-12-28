open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("9.txt")

let stringToIntegers (str: string) =
    str.Split(' ') 
    |> Array.map int
    |> Array.toList

let getDifference (a: int) (b: int) = System.Math.Abs(a-b)

let rec getDiff (numbers: int list) (acc: int list) =
    match numbers with
    | [] -> acc |> List.rev
    | [ x ] -> acc |> List.rev
    | head1 :: head2 :: tail ->
        let diff = getDifference head1 head2
        getDiff (head2 :: tail) (diff :: acc)

let getNextSequence (numbers: int list) = getDiff numbers []


let solve (input: string seq) =
    input
    |> Seq.map stringToIntegers
    |> Seq.map getNextSequence

let testData =
    [
        "0 3 6 9 12 15"
        "1 3 6 10 15 21"
        "10 13 16 21 30 45"
    ]

let answer = testData |> solve
//let answer = readInput () |> solve

printfn "Answer: %A" answer

