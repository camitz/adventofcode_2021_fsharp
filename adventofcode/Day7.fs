module Day7

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day7positions.txt")

let positions = (Seq.exactlyOne lines).Split ','
                |> Seq.map Int32.Parse
                |> Seq.sort

let costToMove p positions =
    positions |> Seq.sumBy (fun x -> abs (x-p))


let puzzle1 = 
    positions
    |> Seq.map (fun p -> costToMove p positions)
    |> Seq.min

