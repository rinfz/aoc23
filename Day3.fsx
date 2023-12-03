open System.IO
open System.Linq
open System
open System.Collections.Generic

let input = File.ReadAllLines "inputs/day3.in"
let engine = Array2D.init input.Length input[0].Length (fun i j -> input[i][j])
let mask = Array2D.create input.Length input[0].Length false
let window: char array2d = Array2D.zeroCreate 2 2

let getNumber y x mark =
    if y < 0 || y >= Array2D.length1 engine || x < 0 || x >= Array2D.length2 engine then
        None
    else if mask[y, x] || not (Char.IsAsciiDigit engine[y, x]) then
        None
    else
        let mutable i = x

        while i > 0 && Char.IsAsciiDigit engine[y, i - 1] do
            i <- i - 1

        let res = engine[y, i..].TakeWhile(fun c -> Char.IsAsciiDigit c)

        if res.Count() > 0 then
            if mark then
                for idx in i .. (i + res.Count() - 1) do
                    Array2D.set mask y idx true

            Some (res.ToArray() |> String |> int)
        else
            None

let numbers = new List<int>()

for y in 0 .. (Array2D.length1 engine) - 2 do
    for x in 0 .. (Array2D.length2 engine) - 2 do
        Array2D.blit engine y x window 0 0 2 2

        let hasSymbol =
            window.Cast<char>().Any(fun c -> not (Char.IsAsciiDigit(c)) && c <> '.')

        if hasSymbol then
            window
            |> Array2D.iteri (fun j i c ->
                match getNumber (y + j) (x + i) true with
                | Some n -> numbers.Add n
                | None -> ())

numbers |> Seq.sum |> printfn "%A"

// reset mask
for y in 0 .. (mask |> Array2D.length1) - 1 do
    for x in 0 .. (mask |> Array2D.length2) - 1 do
        Array2D.set mask y x false

let p2 = new List<int>()
let w2: char array2d = Array2D.zeroCreate 3 3

for y in 0 .. (Array2D.length1 engine) - 3 do
    for x in 0 .. (Array2D.length2 engine) - 3 do
        Array2D.blit engine y x w2 0 0 3 3

        match w2.Cast<char>() |> Seq.tryFindIndex ((=) '*') with
        | Some idx ->
            let nums =
                w2
                |> Array2D.mapi (fun j i _ ->
                    if abs ((idx / 3) - j) < 2 && abs ((idx % 3) - i) < 2 then
                        getNumber (y + j) (x + i) false
                    else
                        None)

            let uniq =
                (nums.Cast<int option>() |> Seq.filter _.IsSome |> Seq.map _.Value |> Set)

            if uniq.Count = 2 then
                p2.Add((1, uniq) ||> Set.fold (fun a b -> a * b))

                // apply mask so we dont consider these numbers again
                w2
                |> Array2D.iteri (fun j i _ ->
                    if abs ((idx / 3) - j) < 2 && abs ((idx % 3) - i) < 2 then
                        getNumber (y + j) (x + i) true |> ignore)
        | None -> ()

p2 |> Seq.sum |> printfn "%A"
