open System
open System.IO
open System.Linq

let input = File.ReadAllLines("inputs/day2.in")
let scores = Map [ ("blue", 14); ("green", 13); ("red", 12) ]

let select (subsets: string array) (fn: int64 -> string -> 'a) =
    seq {
        for subset in subsets do
            let cubes = subset.Split ','

            for cube in cubes do
                let go = cube.Trim().Split ' '
                let count = go[0] |> Int64.Parse
                fn count go[1]
    }

let part1 (line: string) =
    let parts = line.Split ':'
    let id = parts[0].Where(Char.IsAsciiDigit) |> String.Concat |> Int64.Parse
    let subsets = parts[1].Split ';'

    let possible =
        select subsets (fun count colour -> count <= scores[colour])
        |> Seq.forall ((=) true)

    if possible then id else 0L

printfn "%d" (input |> Seq.sumBy part1)

let part2 (line: string) =
    let parts = line.Split ':'
    let subsets = parts[1].Split ';'

    let counts = select subsets (fun count colour -> (colour, count))

    scores.Keys
    |> Seq.map (fun k ->
        counts
        |> Seq.filter (fun cube -> fst cube = k)
        |> Seq.maxBy (fun cube -> snd cube))
    |> Seq.fold (fun a b -> a * snd b) 1L

printfn "%d" (input |> Seq.sumBy part2)
