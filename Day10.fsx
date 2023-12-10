#r "nuget: unquote"

open System.IO
open System.Linq
open System
open System.Collections.Generic
open Swensen.Unquote
open System.Text.RegularExpressions

let filename = "inputs/day10.ex.in"
let input = File.ReadAllLines filename
let shouldTest = filename.Contains("ex")

let findStart (g: char array2d) =
    let rec row' n =
        if n >= Array2D.length1 g then None
        else if g[n,*] |> Array.contains 'S' then Some n
        else row' (n + 1)

    let row = row' 0 |> _.Value
    let rec col' n =
        if n >= Array2D.length2 g then None
        else if g[row,n] = 'S' then Some n
        else col' (n + 1)

    let col = col' 0 |> _.Value
    (row, col)

let grid = Array2D.init input.Length input[0].Length (fun i j -> input[i][j])

let inBounds v min max =
    v >= min && v < max

let yl = Array2D.length1 grid
let xl = Array2D.length2 grid

let followPipe g =
    let q = new Queue<((int * int) * int)>()
    let visited = new HashSet<(int * int)>()
    let distances = new HashSet<int>()

    let start = findStart g
    q.Enqueue (start, 0)
    visited.Add (start) |> ignore

    while not (q.Count = 0) do
        let ((y,x),dist) = q.Dequeue()
        for (xdir,ydir,validC) in [(-1,0,['-';'L';'F']); (0,-1,['|';'7';'F']); (1, 0, ['-'; '7'; 'J']); (0, 1, ['|';'L';'J'])] do
            let nx = x+xdir
            let ny = y+ydir
            let coord = (ny,nx)
            if not (visited.Contains(coord)) && (inBounds nx 0 xl) && (inBounds ny 0 yl) && validC.Contains(g[ny,nx]) then
                visited.Add (coord) |> ignore
                q.Enqueue (coord, dist + 1)
                distances.Add (dist + 1) |> ignore

    distances, visited

let (part1, mainLoop) = followPipe grid

printfn "%A" (part1.Max())
