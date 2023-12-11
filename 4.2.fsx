let log a =
    printfn "%A" a
    a

let readInput () = System.IO.File.ReadLines("4.txt")

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


type CardNr = int
type NrOfCopies = int

let solve (input: string seq) =
    let initCardNrMap : Map<CardNr, NrOfCopies> = Map.empty

    let addToMap (cardNrs: int list) (copies: int) (cardNrMap: Map<CardNr, NrOfCopies>) : Map<CardNr, NrOfCopies> =
        (cardNrMap, cardNrs)
        ||> List.fold 
            (fun acc curr ->
                let existingValueOpt = acc |> Map.tryFind curr

                match existingValueOpt with
                | None -> acc.Add (curr, copies)
                | Some nrOfCopies -> acc.Add (curr, nrOfCopies + copies)
            )

    let (scratchCardsMap, _) =
        ((initCardNrMap, 1), input)
        ||> Seq.fold
            (fun acc curr ->
                let cardNr = snd acc
                let cardNrMap = fst acc |> addToMap [cardNr] 1
                let nrOfMatching = curr |> parseIntoSets |> getMatchingNumbers |> (fun x -> x.Count)
                let cardNrsToAdd = [cardNr+1..cardNr+nrOfMatching]

                let copies = 
                    match cardNrMap.TryFind cardNr with
                    | Some nrOfCopies -> nrOfCopies
                    | None -> 1

                // printfn "cardNr: %i; cardNrMap: %A; nrOfMatching: %i; cardNrsToAdd: %A; copies: %i" cardNr cardNrMap nrOfMatching cardNrsToAdd copies

                let newMap = addToMap cardNrsToAdd copies cardNrMap
                // printfn "newMap: %A" newMap
                let newCardNr = cardNr + 1

                (newMap, newCardNr)
        )
    
    (0, scratchCardsMap)
    ||> Map.fold (fun acc cardNr copies -> acc + copies)

let testData =
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" ]

let result = testData |> solve

printfn "%A" result

//readInput () |> solve