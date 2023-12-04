// ----------------------------------
// Part 1
// ----------------------------------

let readInput () = System.IO.File.ReadLines("1.txt")

let getCalibrationValue (line: string) =
    let charArr = line.ToCharArray()

    let firstNumberOpt = charArr |> Array.tryFind (System.Char.IsDigit)
    let secondNumberOpt = charArr |> Array.tryFindBack (System.Char.IsDigit)

    match firstNumberOpt, secondNumberOpt with
    | Some firstNumber, Some secondNumber ->
        let strArr = [| firstNumber.ToString(); secondNumber.ToString() |]
        System.String.Join("", strArr)
    | _ -> failwith "Fail"
    |> int

let calibrationValues =
    readInput () //|> Seq.map (getCalibrationValue >> int) |> Seq.sum
    |> Seq.sumBy (getCalibrationValue)

printfn "%s" (calibrationValues.ToString())
