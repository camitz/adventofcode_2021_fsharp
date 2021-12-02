module Day1

open System

let readLines filePath = IO.File.ReadLines(filePath) |> Seq.map Double.Parse

let lines = readLines (__SOURCE_DIRECTORY__ + @"\day1readings.txt")
 
let puzzle1 = Seq.pairwise lines 
            |> Seq.map (fun elem -> snd elem > fst elem) 
            |> Seq.sumBy (function |true -> 1 |false -> 0)

let puzzle2 = Seq.windowed 3 lines 
            |> Seq.map Array.sum
            |> Seq.pairwise 
            |> Seq.map (fun elem -> snd elem > fst elem) 
            |> Seq.sumBy (function |true -> 1 |false -> 0)
