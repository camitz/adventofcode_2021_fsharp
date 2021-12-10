module Day8

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day8readings.txt")

let input = fst
let output = snd
let single = Seq.exactlyOne

let readings = lines 
                    |> Seq.map (fun l -> l.Split(" |".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
                    |> Seq.map (fun rs -> (rs.[0..9], rs.[10..]))

let puzzle1 = 
    readings 
    |> Seq.map (fun rs -> output rs |> 
                            Seq.sumBy (fun o -> if o.Length <=4 or o.Length = 7 then 1 else 0))
    |> Seq.sum

let digitsOfLength m =
    Seq.filter (fun (i:string) -> i.Length = m)

let segmentCount =
    Seq.collect id >>
    Seq.groupBy id >>
    Seq.map (fun (s,n) -> (s, Seq.length n))

let deduceConnections inputs =
    let segemenstWithCount m =
        segmentCount inputs
        |> Seq.choose (fun(s,n)-> if n = m then Some s else None)

    [
     yield ("e", segemenstWithCount 4 |> single)
     yield ("b", segemenstWithCount 6 |> single)
     yield ("f", segemenstWithCount 9 |> single)
     //yield ("a", Set.intersect 
     //                   (set(segemenstWithCount 8)) 
     //                   (set(digitsOfLength 2 inputs))
     //               |> single
     //      )
    ]

let puzzle = 
    input (readings |> Seq.head)
    |> segmentCount
    |> Seq.toList

let puzzle2 = 
    input (readings |> Seq.head)
    |> deduceConnections 
    //|> digitsOfLength 2
    |> Seq.toList

