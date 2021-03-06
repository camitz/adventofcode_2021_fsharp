module Day7

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day7positions.txt")

let positions = (Seq.exactlyOne lines).Split ','
                |> Seq.map Int32.Parse
                |> Seq.sort

let costToMove p positions =
    positions |> Seq.sumBy (fun x -> abs (x-p))

let costInc p0 p1 =
    let a = abs (p0-p1) 
    a * (a+1) / 2

let costToMoveInc p1 positions =
    positions 
        |> Seq.sumBy (fun p0 -> costInc p0 p1)

let puzzle1 = 
    positions
    |> Seq.map (fun p -> costToMove p positions)
    |> Seq.min

let puzzle2 = 
    [1..Seq.last positions]
    |> Seq.map (fun p1 -> costToMoveInc p1 positions)
    |> Seq.min

