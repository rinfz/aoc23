// run with dotnet fsi Day1.fsx

open System
open System.IO
open System.Linq

let iad = Char.IsAsciiDigit // why does this have to be separate?

let input = File.ReadAllLines("inputs/day1.in")

let numbersP1 =
    seq {
        for line in input do
            let fst = line.First iad
            let lst = line.Last iad
            Int64.Parse(String.Concat([ fst; lst ]))
    }

printfn "%d" (numbersP1.Sum())

let strNums =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let cmp op (sIdx: int option) (currIdx: int option) =
    sIdx.IsSome && (currIdx.IsNone || op sIdx.Value currIdx.Value)

let p2 (line: string) (s: string option) (n: int option) =
    match s with
    | Some v -> 1 + (strNums |> Seq.findIndex ((=) v))
    | None -> int line[n.Value] - int '0'

let numbersP2 =
    seq {
        for line in input do
            let ((leftN, leftS), (rightN, rightS)) =
                (((Seq.tryFindIndex iad line, None), (Seq.tryFindIndexBack iad line, None)), strNums)
                ||> Seq.fold (fun ((lv, lr), (rv, rr)) sNum ->
                    let strs = line |> Seq.windowed sNum.Length |> Seq.map String
                    let fstS = strs |> Seq.tryFindIndex ((=) sNum)
                    let l = if cmp (<) fstS lv then (fstS, Some sNum) else (lv, lr)
                    let lstS = strs |> Seq.tryFindIndexBack ((=) sNum)
                    let r = if cmp (>) lstS rv then (lstS, Some sNum) else (rv, rr)
                    (l, r))

            let r0 = p2 line leftS leftN
            let r1 = p2 line rightS rightN
            r0 * 10 + r1
    }

printfn "%d" (numbersP2.Sum())
