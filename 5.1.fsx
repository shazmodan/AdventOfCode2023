open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log a =
    printfn "%A" a
    a

let readInput () = System.IO.File.ReadLines("4.txt")

type Mapping = { From: int; To: int; Range: int }

type Maps = 
    { Seeds: int list
      SeedToSoilMap: Mapping list
      SoilToFertilizerMap: Mapping list
      FertilizerToWaterMap: Mapping list
      WaterToLightMap: Mapping list
      LightToTemperatureMap: Mapping list
      TemperatureToHumidityMap: Mapping list
      HumidityToLocationMap: Mapping list }
    static member Default =
        { Seeds = []
          SeedToSoilMap = []
          SoilToFertilizerMap = []
          FertilizerToWaterMap = []
          WaterToLightMap = []
          LightToTemperatureMap = []
          TemperatureToHumidityMap = []
          HumidityToLocationMap = [] }

let parseInputIntoMaps (input: string list) =
    let maps = 
        let row = List.head input
        let numbers = 
            row.Substring(row.IndexOf("seeds: ")).Split(" ")
            |> Array.map int
            |> Array.toList
        
        { Maps.Default with Seeds = numbers }

    let rec parseLines (activeMap: string) (input: string list) (acc: Maps) =
        match List.tryHead input with
        | None -> acc // base case
        | Some head ->
            let newAcc =
                let getMapping (head: string) : Mapping = 
                    match (head.Split( " ") |> Array.toList) with
                    | [ toNr; fromNr; rangeNr ] -> { From = int fromNr; To = int toNr ; Range = int rangeNr }
                    | x -> failwithf "Failed to create mapping with: %A" x

                match activeMap with
                | "" -> acc
                | "seed-to-soil map:" -> { acc with SeedToSoilMap = getMapping head :: acc.SeedToSoilMap }
                | "soil-to-fertilizer map:" -> { acc with SoilToFertilizerMap =  getMapping head :: acc.SoilToFertilizerMap }
                | "fertilizer-to-water map:" -> { acc with FertilizerToWaterMap =  getMapping head :: acc.FertilizerToWaterMap }
                | "water-to-light map:" -> { acc with WaterToLightMap =  getMapping head :: acc.WaterToLightMap }
                | "light-to-temperature map:" -> { acc with LightToTemperatureMap =  getMapping head :: acc.LightToTemperatureMap }
                | "temperature-to-humidity map:" -> { acc with TemperatureToHumidityMap =  getMapping head :: acc.TemperatureToHumidityMap }
                | "humidity-to-location map:" -> { acc with HumidityToLocationMap =  getMapping head :: acc.HumidityToLocationMap }
                | x -> failwithf "Failed to parse activeMap: %s" x
            
            match head.[0] with
            | char when System.Char.IsAsciiLetter(char) ->
                // time to set a new activeMap
                parseLines head (List.tail input) newAcc
            | char when System.Char.IsControl(char) ->
                //newline character discovered
                parseLines "" (List.tail input) newAcc
            | char when System.Char.IsDigit(char) -> 
                parseLines activeMap (List.tail input) newAcc
            | x -> failwithf "Failed to match head.[0]: %c" x
    
    parseLines "" (List.tail input) maps

let solve (input: string seq) =
    let maps = parseInputIntoMaps (input |> Seq.toList)

    maps


let testData =
    [ "seeds: 79 14 55 13"

      "seed-to-soil map:"
      "50 98 2"
      "52 50 48"

      "soil-to-fertilizer map:"
      "0 15 37"
      "37 52 2"
      "39 0 15"

      "fertilizer-to-water map:"
      "49 53 8"
      "0 11 42"
      "42 0 7"
      "57 7 4"

      "water-to-light map:"
      "88 18 7"
      "18 25 70"

      "light-to-temperature map:"
      "45 77 23"
      "81 45 19"
      "68 64 13"

      "temperature-to-humidity map:"
      "0 69 1"
      "1 0 69"

      "humidity-to-location map:"
      "60 56 37"
      "56 93 4" 
    ]

testData |> solve
