open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let log txt a =
    printfn "%s%A" txt a
    a

let readInput () = System.IO.File.ReadLines("7.txt")

type Card =
    | Ace = 14
    | King = 13
    | Queen = 12
    | Jack = 1 // <---  Used to be 11, now 1.
    | Ten = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2

let charToCard =
    function
    | 'A' -> Card.Ace
    | 'K' -> Card.King
    | 'Q' -> Card.Queen
    | 'J' -> Card.Jack
    | 'T' -> Card.Ten
    | '9' -> Card.Nine
    | '8' -> Card.Eight
    | '7' -> Card.Seven
    | '6' -> Card.Six
    | '5' -> Card.Five
    | '4' -> Card.Four
    | '3' -> Card.Three
    | '2' -> Card.Two
    | x -> failwithf "Unrecognized card: %c" x

type HandType =
    | HighCard = 1
    | OnePair = 2
    | TwoPair = 3
    | ThreeOfAKind = 4
    | FullHouse = 5
    | FourOfAKind = 6
    | FiveOfAKind = 7

type NrOfOccurrences = int

type Hand =
    { Cards: Card list
      Bid: int
      HandType: HandType }

let isFiveOfAKind (cardOccurrences: (Card * NrOfOccurrences) list) =
    cardOccurrences |> List.exists (fun (card, nrOfOccurrences) -> nrOfOccurrences = 5)

let isFourOfAKind (cardOccurrences: (Card * NrOfOccurrences) list) =
    cardOccurrences |> List.exists (fun (card, nrOfOccurrences) -> nrOfOccurrences = 4)

let isFullHouse (cardOccurrences: (Card * NrOfOccurrences) list) =
    cardOccurrences |> List.exists (fun (card, nrOfOccurrences) -> nrOfOccurrences = 3)
    && cardOccurrences |> List.exists (fun (card, nrOfOccurrences) -> nrOfOccurrences = 2)

let isThreeOfAKind (cardOccurrences: (Card * NrOfOccurrences) list) =
    cardOccurrences |> List.exists (fun (card, nrOfOccurrences) -> nrOfOccurrences = 3)

let isTwoPair (cardOccurrences: (Card * NrOfOccurrences) list) =
    let nrOf2Occurrences =
        (0, cardOccurrences)
        ||> List.fold(fun acc (card, nrOfOccurrences) -> if nrOfOccurrences = 2 then acc + 1 else acc)

    nrOf2Occurrences = 2

let isOnePair (cardOccurrences: (Card * NrOfOccurrences) list) =
    let (nrOfPairs, nrOfSingleCards) =
        ((0,0), cardOccurrences)
        ||> List.fold
                (fun (nrOfPairs, nrOfSingleCards) (card, nrOfOccurrences) -> 
                    if nrOfOccurrences = 2 then 
                        (nrOfPairs + 1, nrOfSingleCards)
                    elif nrOfOccurrences = 1 then
                        (nrOfPairs, nrOfSingleCards + 1)
                    else 
                        (nrOfPairs, nrOfSingleCards))

    nrOfPairs = 1 && (nrOfSingleCards = cardOccurrences.Length - 1)


let getHandType (cards: Card list) : HandType =
    let cardOccurrences : (Card * NrOfOccurrences) list = cards |> List.countBy id

    if isFiveOfAKind cardOccurrences then
        HandType.FiveOfAKind
    elif isFourOfAKind cardOccurrences then
        HandType.FourOfAKind
    elif isFullHouse cardOccurrences then
        HandType.FullHouse
    elif isThreeOfAKind cardOccurrences then
        HandType.ThreeOfAKind
    elif isTwoPair cardOccurrences then
        HandType.TwoPair
    elif isOnePair cardOccurrences then
        HandType.OnePair
    else
        HandType.HighCard

let getHandTypeWithJacksUpgrade (cards: Card list) =
    let cardsWithoutJacks = (cards |> List.filter (fun card -> card <> Card.Jack))
    let nrOfJacks = 5 - cardsWithoutJacks.Length

    if nrOfJacks = 0 then
        getHandType cards
    else
        let handType = getHandType cardsWithoutJacks

        match handType, nrOfJacks with
        | HandType.FiveOfAKind, _ -> HandType.FiveOfAKind
        | HandType.FourOfAKind, _ -> HandType.FiveOfAKind
        | HandType.FullHouse, 1 -> HandType.FourOfAKind
        | HandType.FullHouse, j when j >= 2 -> HandType.FiveOfAKind
        | HandType.ThreeOfAKind, 1 -> HandType.FourOfAKind
        | HandType.ThreeOfAKind, j when j >= 2 -> HandType.FiveOfAKind
        | HandType.TwoPair, 1 -> HandType.FullHouse
        | HandType.TwoPair, 2 -> HandType.FourOfAKind
        | HandType.TwoPair, j when j >= 3 -> HandType.FiveOfAKind
        | HandType.OnePair, 1 -> HandType.ThreeOfAKind
        | HandType.OnePair, 2 -> HandType.FourOfAKind
        | HandType.OnePair, j when j >= 3 -> HandType.FiveOfAKind
        | HandType.HighCard, 1 -> HandType.OnePair
        | HandType.HighCard, 2 -> HandType.ThreeOfAKind
        | HandType.HighCard, 3 -> HandType.FourOfAKind
        | HandType.HighCard, j when j >= 4 -> HandType.FiveOfAKind
        | _, _ -> failwithf "Couldn't upgrade. HandType: %A, nrOfJacks: %i" handType nrOfJacks

let parseInput (input: string seq) =
    let parseRow (row: string) : Hand =
        let splitted = row.Split(" ")
        let cards = splitted.[0].ToCharArray() |> Array.map charToCard |> Array.toList
        let bid = splitted.[1] |> int

        {
            Cards = cards
            Bid = bid
            HandType = getHandTypeWithJacksUpgrade cards
        }

    input |> Seq.map parseRow

let sortHands (hands: Hand seq) =
    let rec cardByCardComparison (eqCheck: int) (cards: (Card * Card) list) : int =
        if eqCheck = 0 then 
            match cards with
            | [] -> 0
            | (cardA, cardB) :: tail ->
                if cardA > cardB then
                    1
                elif cardA < cardB then
                    -1
                else
                    cardByCardComparison 0 tail
        else eqCheck

    let sortHandsFn (a: Hand) (b: Hand) : int =
        if a.HandType > b.HandType then
            1
        elif a.HandType < b.HandType then
            -1
        else // hands are equally worth, begin card-by-card comparison
            List.zip a.Cards b.Cards
            |> cardByCardComparison 0

    hands |> Seq.sortWith sortHandsFn

let multiplyHandBidWithRank (rank: int) (hand: Hand) = hand.Bid * rank

let solve (input: string seq) =
    parseInput input
    |> sortHands
    |> Seq.toList
    |> Seq.mapi (fun i hand -> multiplyHandBidWithRank (i+1) hand)
    |> Seq.sum

let testData = 
    [
        "32T3K 765"
        "T55J5 684"
        "KK677 28"
        "KTJJT 220"
        "QQQJA 483"
        "22768 633"
        "228JK 466"
        "23323 36"
        "38AJ8 1"
        "JJ7K7 1"
        "79J6J 1"
        "9JJJ9 1"
        "JJJJJ 1"
    ]

let answer = readInput () |> solve
// let answer = testData |> solve

printfn "Answer: %A" answer