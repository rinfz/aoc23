open System
open System.IO
open System.Linq

let r (line: string) =
    line.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.skip 1 |> Array.map int

let input = File.ReadAllLines "inputs/day6.in"

let races = (r input[0], r input[1]) ||> Seq.zip

let part1 = seq {
    for (time, distance) in races do
        yield Seq.sum (seq {
            for t in 0..time do
                let hold = t
                let run = time - t
                if (hold * run) > distance then
                    yield 1
        })
}

printfn "%A" (part1 |> Seq.fold (fun a b -> a * b) 1)

let r' (line: string) =
    line.Where(Char.IsAsciiDigit).ToArray() |> String |> Int64.Parse

let time = r' input[0]
let distance = r' input[1]

let part2 = seq {
    for t in 0L..time do
        let hold = t
        let run = time - t
        if (hold * run) > distance then
            yield 1
}

printfn "%A" (Seq.sum part2)