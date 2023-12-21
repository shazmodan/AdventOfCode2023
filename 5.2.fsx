open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("5test.txt")

type Mapping = { Source: int64; Destination: int64; Range: int64 }

type Maps = 
    { Seeds: int64 list
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
            row.Substring(7).Split(" ")
            |> Array.map int64
            |> Array.toList
        
        { Maps.Default with Seeds = numbers }

    let rec parseLines (activeMap: string) (input: string list) (acc: Maps) =
        match List.tryHead input with
        | None -> acc // base case
        | Some head ->
            let newAcc =
                let getMapping (head: string) : Mapping = 
                    match (head.Split( " ") |> Array.toList) with
                    | [ toNr; fromNr; rangeNr ] -> { Source = int64 fromNr; Destination = int64 toNr ; Range = int64 rangeNr }
                    | x -> failwithf "Failed to create mapping with: %A" x

                match activeMap = head || head = "" with
                | true -> acc
                | false ->
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

            match head = "" with
            | true -> parseLines "" (List.tail input) newAcc
            | false ->
                match head.[0] with
                | char when System.Char.IsAsciiLetter(char) -> parseLines head (List.tail input) newAcc
                | char when System.Char.IsDigit(char) -> parseLines activeMap (List.tail input) newAcc
                | x -> failwithf "Failed to match head.[0]: %c" x
    
    parseLines "" (List.tail input) maps

let isWithinRange (rangeStart: int64) (rangeLength: int64) (number: int64) =
    number >= rangeStart && number <= (rangeStart + rangeLength - 1L)

let getDestinationNumber (mapping: Mapping) (number: int64) =
    match isWithinRange mapping.Source mapping.Range number with
    | false -> None
    | true -> number + (mapping.Destination - mapping.Source) |> Some

let getDestinationNumberFromMappings (mappings: Mapping list) (number: int64) =
    mappings
    |> List.tryPick (fun mapping -> getDestinationNumber mapping number)
    |> Option.defaultValue number

let getSourceDestinationChain (mappings: Mapping list list) (numberStart: int64) =
    (numberStart, mappings)
    ||> List.scan (fun acc curr -> getDestinationNumberFromMappings curr acc)

let solve (input: string seq) =
    let maps = parseInputIntoMaps (input |> Seq.toList)

    let mapsLst = 
        [ maps.SeedToSoilMap; maps.SoilToFertilizerMap; maps.FertilizerToWaterMap; maps.WaterToLightMap; maps.LightToTemperatureMap; maps.TemperatureToHumidityMap; maps.HumidityToLocationMap ]
    
    let rec createSeedAndMaxValue (acc: (int64 * int64) list) (lst: int64 list) : (int64 * int64) list =
        match lst with
        | [] -> acc |> List.rev
        | a :: b :: tail -> createSeedAndMaxValue ((a,a + b - 1L) :: acc) tail
        | x -> failwithf "Cannot createSeedPairs with this: %A" x

    let sourceDestinationChains =
        let generateSeeds = 
            Seq.unfold 
                (fun (seed, maxValue) -> 
                    if seed > maxValue then 
                        None 
                    else
                        Some ((seed, maxValue), (seed + 1L, maxValue))
                )

        let seeds =
            maps.Seeds
            |> createSeedAndMaxValue []
            |> Seq.map generateSeeds
            |> Seq.concat

        seeds
        |> Seq.map (fun (seed, maxValue) -> getSourceDestinationChain mapsLst seed)

    let lowestLastNumbers =
        ([], sourceDestinationChains)
        ||> Seq.fold (fun acc curr -> (curr |> Seq.last) :: acc)
    
    List.min lowestLastNumbers


let testData =
    [ "seeds: 79 14 55 13"
      ""
      "seed-to-soil map:"
      "50 98 2"
      "52 50 48"
      ""
      "soil-to-fertilizer map:"
      "0 15 37"
      "37 52 2"
      "39 0 15"
      ""
      "fertilizer-to-water map:"
      "49 53 8"
      "0 11 42"
      "42 0 7"
      "57 7 4"
      ""
      "water-to-light map:"
      "88 18 7"
      "18 25 70"
      ""
      "light-to-temperature map:"
      "45 77 23"
      "81 45 19"
      "68 64 13"
      ""
      "temperature-to-humidity map:"
      "0 69 1"
      "1 0 69"
      ""
      "humidity-to-location map:"
      "60 56 37"
      "56 93 4" 
    ]


testData |> solve
//readInput () |> solve
