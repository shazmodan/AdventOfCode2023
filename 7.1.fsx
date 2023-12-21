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
    | Jack = 11
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
    cardOccurrences.Length = 1

let isFourOfAKind (cardOccurrences: (Card * NrOfOccurrences) list) =
    match cardOccurrences with
    | [ (_, occurrenceA); (_, occurrenceB) ] -> occurrenceA = 4 || occurrenceB = 4
    | _ -> false

let isFullHouse (cardOccurrences: (Card * NrOfOccurrences) list) =
    match cardOccurrences with
    | [ (_, occurrenceA); (_, occurrenceB) ] -> 
        (occurrenceA = 3 && occurrenceB = 2) || (occurrenceA = 2 && occurrenceB = 3)
    | _ -> false

let isThreeOfAKind (cardOccurrences: (Card * NrOfOccurrences) list) =
    match cardOccurrences with
    | [ (_, occurrenceA); (_, occurrenceB); (_, occurrenceC) ] -> 
        (occurrenceA = 3 && occurrenceB = 1 && occurrenceC = 1) 
        || (occurrenceA = 1 && occurrenceB = 3 && occurrenceC = 1) 
        || (occurrenceA = 1 && occurrenceB = 1 && occurrenceC = 3)
    | _ -> false

let isTwoPair (cardOccurrences: (Card * NrOfOccurrences) list) =
    match cardOccurrences with
    | [ (_, occurrenceA); (_, occurrenceB); (_, occurrenceC) ] -> 
        (occurrenceA = 2 && occurrenceB = 2)
        || (occurrenceA = 2 && occurrenceC = 2)
        || (occurrenceB = 2 && occurrenceC = 2)
    | _ -> false

let isOnePair (cardOccurrences: (Card * NrOfOccurrences) list) =
    let occurancesWithoutPair =
        cardOccurrences |> List.filter (fun (card, occurrence) -> occurrence <> 2)

    match occurancesWithoutPair.Length = 3 with
    | false -> false
    | true -> occurancesWithoutPair |> List.forall (fun (card, occurrence) -> occurrence = 1)

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

let parseInput (input: string seq) =
    let parseRow (row: string) : Hand =
        let splitted = row.Split(" ")
        let cards = splitted.[0].ToCharArray() |> Array.map charToCard |> Array.toList
        let bid = splitted.[1] |> int

        {
            Cards = cards
            Bid = bid
            HandType = getHandType cards
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
        // "22768 633"
        // "228JK 466"
        // "23323 36"
    ]

let answer = readInput () |> solve
// let answer = testData |> solve

printfn "Answer: %A" answer