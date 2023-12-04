open System
open System.IO
open System.Linq
open System.Collections.Generic

let input = File.ReadAllLines "inputs/day4.in"

let parseNumbers (part: string) =
    part.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let part1 (line: string) =
    let data = line.Split ':'
    let numbers = data[1].Split('|')
    let winning = parseNumbers numbers[0]
    let owned = parseNumbers numbers[1]
    let exp = owned.Count(fun n -> winning.Contains n) - 1
    pown 2 exp

printfn "%A" (input |> Seq.sumBy part1)

type Card(id: int, winning: int array, owned: int array) =
    let mutable wins = None
    member val Id = id
    member val Winning = winning
    member val Owned = owned

    member this.Wins() =
        match wins with
        | None ->
            let res = this.Owned.Count(fun n -> this.Winning.Contains n)
            wins <- Some res
            res
        | Some v -> v

let stack = new Stack<Card>()
let cards = new Dictionary<int, Card>()
let counts = new Dictionary<int, int>() // could remove this and track on Card

for line in input do
    let data = line.Split ':'
    let id = int (data[0].Split(" ", StringSplitOptions.RemoveEmptyEntries)[1])
    let numbers = data[1].Split('|')
    let winning = parseNumbers numbers[0]
    let owned = parseNumbers numbers[1]
    counts.Add(id, 0)
    let card = new Card(id, winning, owned)
    stack.Push card
    cards.Add(id, card)

while stack.Count > 0 do
    let card = stack.Pop()
    counts[card.Id] <- counts[card.Id] + 1
    let wins = card.Wins()

    for i in 1..wins do
        stack.Push cards[card.Id + i]

printfn "%A" (counts.Values.Sum())
