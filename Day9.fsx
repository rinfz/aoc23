#r "nuget: unquote"

open System.IO
open Swensen.Unquote

let filename = "inputs/day9.in"
let doTest = filename.Contains("ex")

let input =
    File.ReadAllLines filename |> Array.map (fun l -> l.Split() |> Array.map int)

let takeUntil predicate (source: seq<_>) =
    seq {
        use e = source.GetEnumerator()
        let mutable latest = Unchecked.defaultof<_>

        while e.MoveNext()
              && (latest <- e.Current
                  not (predicate latest)) do
            yield latest

        yield latest
    }

let adjacentDiff (xs: int array) =
    xs |> Array.windowed 2 |> Array.map (fun x -> x[1] - x[0])

let diffAll (xs: int array) =
    let mutable res = xs

    Seq.initInfinite (fun _ ->
        res <- adjacentDiff res
        res)
    |> takeUntil (fun s -> Seq.forall (fun s' -> s' = 0) s)

let nextValue (xs: int array) =
    diffAll xs
    |> Seq.rev
    |> Seq.fold (fun a b -> a + Array.last b) 0
    |> (+) (Array.last xs)

let prevValue (xs: int array) =
    diffAll xs
    |> Seq.rev
    |> Seq.fold (fun a b -> Array.head b - a) 0
    |> (-) (Array.head xs)

let part1 = input |> Array.map nextValue |> Array.sum
printfn "%A" part1

let part2 = input |> Array.map prevValue |> Array.sum
printfn "%A" part2

if doTest then
    test <@ adjacentDiff [| 0; 3; 6; 9; 12; 15 |] = [| 3; 3; 3; 3; 3 |] @>
    test <@ adjacentDiff [| 3; 3; 3; 3; 3 |] = [| 0; 0; 0; 0 |] @>
    // weird bugs with unquote
    let t0 = diffAll [| 0; 3; 6; 9; 12; 15 |] |> Seq.toArray
    test <@ t0[0] = [| 3; 3; 3; 3; 3 |] @>
    test <@ t0[1] = [| 0; 0; 0; 0 |] @>

    test <@ nextValue [| 0; 3; 6; 9; 12; 15 |] = 18 @>
    test <@ nextValue [| 1; 3; 6; 10; 15; 21 |] = 28 @>

    test <@ prevValue [| 10; 13; 16; 21; 30; 45 |] = 5 @>
