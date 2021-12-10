module Day8

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day8readings.txt")

let input = fst
let output = snd

let readings = lines 
                    |> Seq.map (fun l -> l.Split(" |".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
                    |> Seq.map (fun rs -> (rs.[0..9], rs.[10..]))

let puzzle1 = 
    readings 
    |> Seq.map (fun rs -> output rs |> 
                            Seq.sumBy (fun o -> if o.Length <=4 or o.Length = 7 then 1 else 0))
    |> Seq.sum

let segmentCount inputs =
    inputs
    |> Seq.collect id
    |> Seq.groupBy id
    |> Seq.map (fun (s,n) -> (s, Seq.length n))

let puzzle2 = 
    (input >> segmentCount) (readings |> Seq.head)
    |> Seq.toList

