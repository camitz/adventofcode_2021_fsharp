module Day8

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day8readings.txt")

let readings = lines |>
                    Seq.map (fun l -> l.Split(" |".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))

let puzzle1 = 
    readings 
    |> Seq.map (fun rs -> rs.[10..])
    |> Seq.map (fun os -> os|> 
                            Seq.sumBy (fun o -> if o.Length <=4 or o.Length = 7 then 1 else 0))
    |> Seq.sum

let puzzle2 = 1

