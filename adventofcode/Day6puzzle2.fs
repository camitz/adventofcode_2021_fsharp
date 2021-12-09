module Day6Puzzle2

open System 

type long = int64

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day6ages.txt")

let ages = (Seq.exactlyOne lines).Split ','
        |> Seq.map Int32.Parse
        |> Seq.toList

let ageBins =
    [|0..8|] 
        |> Array.map (fun b -> int64 (ages |> List.filter (fun a -> a=b) |> List.length))

let growTo (m : int) : long[] =
    let rec grow day (a:long[]) =
        if day >= m then
            a
        else
            grow (day+1) 
                (Array.append a.[1..6] [|a.[7]+a.[0]; a.[8]; a.[0]|])

    grow 1 ageBins

let puzzle2 = 
    //[1..81] |> List.iter (fun m ->
    //    printfn "%A" (growTo m))
    growTo 257 |> Array.sum
