#r "nuget: unquote"

open System.IO
open System.Linq
open Swensen.Unquote

let inputFile = "inputs/day7.in"
let shouldTest = inputFile.Contains("ex")

type Kind =
    | Five
    | Four
    | Full
    | Three
    | Two
    | One
    | High

type Hand = { Cards: string; Bid: int }

let parseLine (line: string) : Hand =
    let parts = line.Split()

    { Cards = parts[0]
      Bid = int (parts[1].Trim()) }

let input = File.ReadAllLines inputFile |> Array.map parseLine

let identity a = a

let groupByIdentity (cards: string) = cards.GroupBy(identity)

let isFive (hand: Hand) = hand.Cards.Distinct().Count() = 1

let isFour (hand: Hand) =
    hand.Cards
    |> groupByIdentity
    |> Seq.tryFind (fun g -> g.Count() = 4)
    |> Option.isSome

let isFullHouse (hand: Hand) =
    let groups = groupByIdentity hand.Cards
    let count = Seq.length groups

    if count <> 2 then
        false
    else
        let l = groups.ToList()
        let len1 = Seq.length l[0]
        let len2 = Seq.length l[1]
        (len1 = 2 && len2 = 3) || (len1 = 3 && len2 = 2)

let isThree (hand: Hand) =
    hand.Cards
    |> groupByIdentity
    |> Seq.tryFind (fun g -> g.Count() = 3)
    |> Option.isSome

let isTwo (hand: Hand) =
    hand.Cards
    |> groupByIdentity
    |> Seq.countBy (fun g -> g.Count() = 2)
    |> Seq.tryFind ((=) (true, 2))
    |> Option.isSome

let isOne (hand: Hand) =
    hand.Cards
    |> groupByIdentity
    |> Seq.countBy (fun g -> g.Count() = 2)
    |> Seq.tryFind ((=) (true, 1))
    |> Option.isSome

let handSortKey (ranking: string) (hand: Hand) =
    hand.Cards.ToCharArray() |> Array.mapi (fun i c -> (i, ranking.IndexOf(c)))

let handType (hand: Hand) =
    if isFive hand then Five
    else if isFour hand then Four
    else if isFullHouse hand then Full
    else if isThree hand then Three
    else if isTwo hand then Two
    else if isOne hand then One
    else High

let rankHands (f1: Hand -> Kind) (f2: Hand -> ((int * int) array)) (hands: Hand array) =
    hands
    |> Array.groupBy f1
    |> Array.sortBy fst
    |> Array.map (fun g -> g |> snd |> Array.sortBy f2 |> Array.rev)
    |> Array.reduce Array.append
    |> Array.rev

let part1 hands =
    hands
    |> rankHands handType (handSortKey "23456789TJQKA")
    |> Array.mapi (fun i h -> (i + 1) * h.Bid)
    |> Array.sum

printfn "%A" (part1 input)

let modeChar (cards: string) =
    let groups =
        cards.Replace("J", "").Replace("A", "")
        |> groupByIdentity
        |> Seq.sortBy (fun g -> (g.Count(), "AKQT98765432J".IndexOf(g.First())))

    if Seq.length groups = 0 then
        'J'
    else
        groups |> Seq.last |> Seq.head

let handType' (hand: Hand) =
    if hand.Cards.Contains('J') then
        // either replace jokers with aces
        let cardsA = hand.Cards.Replace('J', 'A')
        // or find the most common occurring char and replace jokers with that
        let cardsB = hand.Cards.Replace('J', modeChar hand.Cards)
        // min because the order of the Kind type is inverted (unintentionally!)
        min (handType { Cards = cardsB; Bid = hand.Bid }) (handType { Cards = cardsA; Bid = hand.Bid })
    else
        handType hand

let part2 hands =
    hands
    |> rankHands handType' (handSortKey "J23456789TQKA")
    |> Array.mapi (fun i h -> (i + 1) * h.Bid)
    |> Array.sum

printfn "%A" (part2 input)

if shouldTest then
    test <@ handType' { Cards = "KTJJT"; Bid = 0 } = Four @>

    test <@ isFive { Cards = "11111"; Bid = 0 } = true @>
    test <@ isFive { Cards = "11112"; Bid = 0 } = false @>
    test <@ isFour { Cards = "11112"; Bid = 0 } = true @>
    test <@ isFour { Cards = "11111"; Bid = 0 } = false @>
    test <@ isFour { Cards = "12334"; Bid = 0 } = false @>
    test <@ isFullHouse { Cards = "11222"; Bid = 0 } = true @>
    test <@ isFullHouse { Cards = "11111"; Bid = 0 } = false @>
    test <@ isFullHouse { Cards = "11345"; Bid = 0 } = false @>
    test <@ isThree { Cards = "11123"; Bid = 0 } = true @>
    test <@ isThree { Cards = "11223"; Bid = 0 } = false @>
    test <@ isTwo { Cards = "23432"; Bid = 0 } = true @>
    test <@ isTwo { Cards = "23232"; Bid = 0 } = false @>
    test <@ isOne { Cards = "A23A4"; Bid = 0 } = true @>
    test <@ isOne { Cards = "A23AA"; Bid = 0 } = false @>
    test <@ isOne { Cards = "A2345"; Bid = 0 } = false @>

    test
        <@
            compare
                (handSortKey "23456789TJQKA" { Cards = "33332"; Bid = 0 })
                (handSortKey "23456789TJQKA" { Cards = "2AAAA"; Bid = 0 }) = 1
        @>

    test
        <@
            compare
                (handSortKey "23456789TJQKA" { Cards = "77888"; Bid = 0 })
                (handSortKey "23456789TJQKA" { Cards = "77788"; Bid = 0 }) = 1
        @>

    test
        <@
            compare
                (handSortKey "23456789TJQKA" { Cards = "QQQJA"; Bid = 0 })
                (handSortKey "23456789TJQKA" { Cards = "T55J5"; Bid = 0 }) = 1
        @>

    test
        <@
            input
            |> Array.groupBy handType
            |> Array.sortBy fst
            |> Array.map (fun g -> g |> snd |> Array.sortBy (handSortKey "23456789TJQKA") |> Array.rev) = [| [| { Cards =
                                                                                                                    "QQQJA"
                                                                                                                  Bid =
                                                                                                                    483 }
                                                                                                                { Cards =
                                                                                                                    "T55J5"
                                                                                                                  Bid =
                                                                                                                    684 } |]
                                                                                                             [| { Cards =
                                                                                                                    "KK677"
                                                                                                                  Bid =
                                                                                                                    28 }
                                                                                                                { Cards =
                                                                                                                    "KTJJT"
                                                                                                                  Bid =
                                                                                                                    220 } |]
                                                                                                             [| { Cards =
                                                                                                                    "32T3K"
                                                                                                                  Bid =
                                                                                                                    765 } |] |]
        @>

    test
        <@
            rankHands handType (handSortKey "23456789TJQKA") input = [| { Cards = "32T3K"; Bid = 765 }
                                                                        { Cards = "KTJJT"; Bid = 220 }
                                                                        { Cards = "KK677"; Bid = 28 }
                                                                        { Cards = "T55J5"; Bid = 684 }
                                                                        { Cards = "QQQJA"; Bid = 483 } |]
        @>
