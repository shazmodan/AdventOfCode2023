// ----------------------------------
// Part 2
// ----------------------------------

let validStringsMap =
    [ "one", "1"
      "two", "2"
      "three", "3"
      "four", "4"
      "five", "5"
      "six", "6"
      "seven", "7"
      "eight", "8"
      "nine", "9"
      "zero", "0"
      "0", "0"
      "1", "1"
      "2", "2"
      "3", "3"
      "4", "4"
      "5", "5"
      "6", "6"
      "7", "7"
      "8", "8"
      "9", "9" ]
    |> Map.ofList

type IndexValue =
    { Index: int
      Value: string }

    static member MinDefault =
        { Index = System.Int32.MaxValue
          Value = "" }

    static member MaxDefault =
        { Index = System.Int32.MinValue
          Value = "" }

let getCalibrationValue2 (line: string) =
    let (minIndexValue, maxIndexValue) =
        ((IndexValue.MinDefault, IndexValue.MaxDefault), validStringsMap)
        ||> Map.fold (fun acc key value ->
            let firstIndex = line.IndexOf key
            let lastIndex = line.LastIndexOf key
            let minIndex = (fst acc).Index
            let maxIndex = (snd acc).Index

            let getIndexValue (index: int) =
                if index < minIndex && index > maxIndex then
                    // This case should occur once when we find the first match
                    ({ Index = index; Value = value }, { Index = index; Value = value })
                elif index < minIndex then
                    // We have a new min index
                    ({ Index = index; Value = value }, snd acc)
                elif index > maxIndex then
                    // We have a new max index
                    (fst acc, { Index = index; Value = value })
                else
                    acc

            match firstIndex <> -1 with
            | true ->
                let index1 = getIndexValue firstIndex
                let index2 = getIndexValue lastIndex

                let newMin =
                    match (fst index1).Index <= (snd index2).Index with
                    | true -> index1
                    | false -> index2

                let newMax =
                    match (fst index1).Index >= (snd index2).Index with
                    | true -> index1
                    | false -> index2

                (fst newMin, snd newMax)
            | false -> acc)

    sprintf "%s%s" minIndexValue.Value maxIndexValue.Value


// let testData () =
//     [ "two1nine"
//       "eightwothree"
//       "abcone2threexyz"
//       "xtwone3four"
//       "4nineeightseven2"
//       "zoneight234"
//       "7pqrstsixteen"
//       "eighthree"
//       "sevenine" ]
//     |> List.map (getCalibrationValue2 >> int)

// let testData2 () =
//     [ "sixthree8sixjxjqsjgjgp" // 68 = wrong, 66 = right
//       "bbm4twoeight8oneone3one" // 43 = wrong, 41 = right
//       "nineninesix6nine" ] // 96 = wrong, 99 = right
//     |> List.map (getCalibrationValue2 >> int)

// let tofile () =
//     System.IO.File.ReadLines("1.txt")
//     |> Seq.map getCalibrationValue2
//     |> (fun strArr -> System.IO.File.WriteAllLines("1wrong.txt", strArr))

// tofile ()

let calibrationValues2 =
    System.IO.File.ReadLines("1.txt") |> Seq.sumBy (getCalibrationValue2 >> int)

printfn "%s" (calibrationValues2.ToString())
