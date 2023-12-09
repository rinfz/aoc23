#r "nuget: unquote"

open System.IO
open Swensen.Unquote
open System.Text.RegularExpressions

let inputFile = "inputs/day8.in"
let shouldTest = inputFile.Contains("ex")

let re = new Regex(@"([0-9A-Z]{3}) = \(([0-9A-Z]{3}), ([0-9A-Z]{3})\)")

let parseNetworkLine (line: string) =
    let groups = re.Match(line).Groups
    (groups[1].Value, (groups[2].Value, groups[3].Value))

let input = File.ReadAllText inputFile |> _.Split("\n\n")
let directions = input[0]
let network = input[1].Trim().Split("\n") |> Array.map parseNetworkLine |> Map

if shouldTest then
    test <@ parseNetworkLine "AAA = (BBB, CCC)" = ("AAA", ("BBB", "CCC")) @>

let dir c = if c = 'L' then fst else snd

let mutable i = 0
let mutable curr = "AAA"

while curr <> "ZZZ" do
    curr <- dir directions[i % directions.Length] network[curr]
    i <- i + 1

printfn "%A" i

let mutable nodes =
    input[1].Trim().Split("\n")
    |> Array.filter (fun l -> l[2] = 'A')
    |> Array.map (fun l -> l.Split()[0])

let part2 =
    seq {
        for n' in nodes do
            let mutable i = 0
            let mutable n = n'

            while not (n.EndsWith('Z')) do
                n <- dir directions[i % directions.Length] network[n]
                i <- i + 1

            yield int64 i
    }

let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

printfn "%A" (part2 |> Seq.fold lcm 1)
