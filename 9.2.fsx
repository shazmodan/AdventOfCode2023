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

let rec getDifferences (numbers: int list) (acc: int list) =
    match numbers with
    | [] -> acc
    | [ x ] -> acc
    | head1 :: head2 :: tail ->
        let diff = head1 - head2
        getDifferences (head2 :: tail) (diff :: acc)

let getAllDiffSequences (numbers: int list) = 
    let sequences =
        let generator (numbers: int list) =
            if numbers |> List.forall ((=) 0) then
                None
            else
                let diffs = [] |> getDifferences numbers |> List.rev
                Some(diffs, diffs)

        List.unfold generator (numbers |> List.rev)

    (numbers |> List.rev) :: sequences

let climbUpTheTree (numbers: int list list) =
    (0, numbers)
    ||> List.fold (fun acc curr -> curr.Head + acc)

let solve (input: string seq) =
    input
    |> Seq.map (stringToIntegers >> List.rev >> getAllDiffSequences >> List.rev >> climbUpTheTree)
    |> Seq.sum

let testData =
    [
        // "0 3 6 9 12 15"
        // "1 3 6 10 15 21"
        // "10 13 16 21 30 45"
        //"-6 -3 0 3 6 9"
        //"-10 -13 -16 -21 -30 -45"
    ]

//let answer = testData |> solve
let answer = readInput () |> solve

printfn "Answer: %A" answer

