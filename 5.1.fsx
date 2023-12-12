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

let parseInputIntoMaps (input: string seq) =


let solve (input: string seq) =
    (Maps.Default, input)
    ||> Seq.fold
            (fun acc curr -> 
                
            )


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
      "56 93 4" ]
