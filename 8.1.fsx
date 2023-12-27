open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("8.txt")

type Node = { Name: string; Left: string; Right: string }
type Network = {
    Instructions: char list
    ElementDict: System.Collections.Generic.Dictionary<string, Node>
}

let parseInput (input: string seq) =
    let elementDict = System.Collections.Generic.Dictionary<string, Node> ()
    let mutable instructions = []

    let parseRow (row: string) =
        let nodeName = row.Substring(0, 3)
        let left = row.Substring(7, 3)
        let right = row.Substring(12, 3)

        (nodeName, left, right)

    input 
    |> Seq.iteri 
        (fun i row ->
            if i = 0 then
                instructions <- row.ToCharArray() |> Array.toList
            elif row = "" then
                () //skip empty row
            else
                let (nodeName, left, right) = parseRow row
                elementDict.Add(nodeName, { Name = nodeName; Left = left; Right = right })
        )

    { Instructions = instructions; ElementDict = elementDict}

let getRequiredNumberOfSteps (network: Network) =
    let networkInstructions = network.Instructions
    let elementDict = network.ElementDict
    let startNode = elementDict["AAA"]

    let rec getSteps (currentNode: Node) (instructions: char list) (nrOfSteps: int) =
        match currentNode.Name with
        | "ZZZ" ->
            nrOfSteps // terminal condition
        | _ ->
            match instructions with
            | [] ->
                getSteps currentNode networkInstructions nrOfSteps // reset instructions
            | currentInstruction :: remainingInstructions ->
                let nextNode =
                    match currentInstruction with
                    | 'L' -> elementDict[currentNode.Left]
                    | 'R' -> elementDict[currentNode.Right]
                    | x -> failwithf "Unrecognized currentInstruction: '%c'" x

                getSteps nextNode remainingInstructions (nrOfSteps + 1)

    getSteps startNode networkInstructions 0

let solve (input: string seq) = 
    let network = input |> parseInput
    
    getRequiredNumberOfSteps network

let testData =
    [
        "LLR"
        ""
        "AAA = (BBB, BBB)"
        "BBB = (AAA, ZZZ)"
        "ZZZ = (ZZZ, ZZZ)"
    ]

let testData2 =
    [
        "LR"
        ""
        "AAA = (BBB, CCC)"
        "BBB = (DDD, EEE)"
        "CCC = (ZZZ, GGG)"
        "DDD = (DDD, DDD)"
        "EEE = (GGG, EEE)"
        "GGG = (GGG, ZZZ)"
        "ZZZ = (ZZZ, ZZZ)"
    ]

//let answer = testData |> solve
// let answer = testData2 |> solve
let answer = readInput () |> solve

printfn "Answer: %A" answer