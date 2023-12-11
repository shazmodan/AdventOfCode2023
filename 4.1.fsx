let log a =
    printfn "%A" a
    a

let readInput () = System.IO.File.ReadLines("4.txt")

let doubleIt (numbers: Set<int>) =
    (0, numbers) ||> Set.fold (fun acc _ -> if acc = 0 then 1 else acc * 2)

let getMatchingNumbers (winningNumbers: Set<int>, yourNumbers: Set<int>) =
    Set.intersect winningNumbers yourNumbers

let parseIntoSets (row: string) =
    let rowPartIntoSet (rowPart: string) =
        (([]: int list), rowPart.Split ' ')
        ||> Array.fold (fun acc curr -> if curr = "" then acc else int curr :: acc)
        |> Set.ofList

    let afterColonIndex = (row.IndexOf ':') + 1
    let verticalBarIndex = (row.IndexOf '|')

    let winningNumbersSet =
        row.Substring(afterColonIndex + 1, verticalBarIndex - afterColonIndex - 2)
        |> rowPartIntoSet

    let yourNumbersSet = row.Substring(verticalBarIndex + 2) |> rowPartIntoSet

    (winningNumbersSet, yourNumbersSet)


let solve (input: string seq) =
    (0, input)
    ||> Seq.fold (fun acc curr -> acc + (curr |> (parseIntoSets >> getMatchingNumbers >> doubleIt)))


let testData =
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" ]

// testData |> solve

readInput () |> solve
