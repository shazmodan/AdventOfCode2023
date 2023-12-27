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
    Instructions: char array
    ElementDict: System.Collections.Generic.Dictionary<string, Node>
}

let parseInput (input: string seq) =
    let elementDict = System.Collections.Generic.Dictionary<string, Node> ()
    let mutable instructions = [||]

    let parseRow (row: string) =
        let nodeName = row.Substring(0, 3)
        let left = row.Substring(7, 3)
        let right = row.Substring(12, 3)

        (nodeName, left, right)

    input 
    |> Seq.iteri 
        (fun i row ->
            if i = 0 then
                instructions <- row.ToCharArray()
            elif row = "" then
                () //skip empty row
            else
                let (nodeName, left, right) = parseRow row
                elementDict.Add(nodeName, { Name = nodeName; Left = left; Right = right })
        )

    { Instructions = instructions; ElementDict = elementDict}

// let getRequiredNumberOfSteps (startNode: Node) (network: Network) =
//     let networkInstructions = network.Instructions
//     let elementDict = network.ElementDict

//     let rec getSteps (currentNode: Node) (instructions: char list) (nrOfSteps: int) =
//         match currentNode.Name with
//         | x when x.EndsWith "Z" -> (currentNode, nrOfSteps) // terminal condition
//         | _ ->
//             match instructions with
//             | [] ->
//                 getSteps currentNode networkInstructions nrOfSteps // reset instructions
//             | currentInstruction :: remainingInstructions ->
//                 let nextNode =
//                     match currentInstruction with
//                     | 'L' -> elementDict[currentNode.Left]
//                     | 'R' -> elementDict[currentNode.Right]
//                     | x -> failwithf "Unrecognized currentInstruction: '%c'" x

//                 getSteps nextNode remainingInstructions (nrOfSteps + 1)

//     getSteps startNode networkInstructions 0

// let getSteps (currentNode: Node) (instructions: char list) (nrOfSteps: int) =
//     match currentNode.Name with
//     | x when x.EndsWith "Z" -> (currentNode, nrOfSteps) // terminal condition
//     | _ ->
//         match instructions with
//         | [] ->
//             getSteps currentNode networkInstructions nrOfSteps // reset instructions
//         | currentInstruction :: remainingInstructions ->
//             let nextNode =
//                 match currentInstruction with
//                 | 'L' -> elementDict[currentNode.Left]
//                 | 'R' -> elementDict[currentNode.Right]
//                 | x -> failwithf "Unrecognized currentInstruction: '%c'" x

//             getSteps nextNode remainingInstructions (nrOfSteps + 1)

let solve (input: string seq) : int = 
    let network = input |> parseInput
    
    let startNodes = 
        let mutable en = network.ElementDict.GetEnumerator ()
        let keyValuePairs =
            Seq.unfold 
                (fun _ -> 
                    if en.MoveNext() then
                        Some(en.Current, ())
                    else
                        en.Dispose()
                        None)
                ()

        ([], keyValuePairs)
        ||> Seq.fold 
                (fun acc curr ->
                    if curr.Key.EndsWith 'A' then
                        curr.Value :: acc
                    else
                        acc)

    let instructions = network.Instructions

    let getNextNode (instruction: char) (node: Node) =
        match instruction with
        | 'L' -> node.Left
        | 'R' -> node.Right
        | x -> failwithf "Unrecognized instruction: '%c'" x
        |> fun key -> network.ElementDict[key]

    let rec moveForward (nodes: Node list) (nrOfSteps: int) =
        match nodes |> List.forall (fun node -> node.Name.EndsWith 'Z') with 
        | true -> nrOfSteps // terminal condition
        | false ->
            let currentInstruction = instructions[nrOfSteps % instructions.Length]
            let nextNodes = nodes |> List.map (getNextNode currentInstruction)

            // printfn "currentInstruction: %c, nextNodes: %A" currentInstruction (nextNodes |> List.map (fun x -> x.Name))

            moveForward nextNodes (nrOfSteps + 1)
    
    moveForward startNodes 0


let testData =
    [
        "LR"
        ""
        "11A = (11B, XXX)"
        "11B = (XXX, 11Z)"
        "11Z = (11B, XXX)"
        "22A = (22B, XXX)"
        "22B = (22C, 22C)"
        "22C = (22Z, 22Z)"
        "22Z = (22B, 22B)"
        "XXX = (XXX, XXX)"
    ]

//let answer = testData |> solve
let answer = readInput () |> solve

printfn "Answer: %A" answer