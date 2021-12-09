module Day6

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day6ages.txt")

let ages = (Seq.exactlyOne lines).Split ','
        |> Seq.map Int32.Parse
        |> Seq.toList

let growTo m =
    let rec grow day fish =
        if day >= m then
            fish
        else
            grow (day+1)
                ((fish |> List.map (fun a -> if a=0 then 6 else a-1)) @
                (fish 
                    |> List.filter (fun a -> a=0) 
                    |> List.map (fun _ -> 8)))
    grow 1 ages

let puzzle1 = 
    [1..7] |> List.iter (fun m ->
        printfn "%A" (growTo m))
    growTo 81 |> List.length
