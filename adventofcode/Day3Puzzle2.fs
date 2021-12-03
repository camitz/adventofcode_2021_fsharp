module Day3Puzzle2

open System 

let readLines filePath = IO.File.ReadLines filePath 
                        |> Seq.map (fun n -> Convert.ToUInt64(n, 2))

let lines = readLines (__SOURCE_DIRECTORY__ + @"\day3diagnostics.txt")

let sumWithMask ns mask = ns 
                            |> Seq.map (fun n -> mask &&& n)
                            |> Seq.sum 

let filter bitValuef mask s  = 
    let bitSum = sumWithMask s mask / mask
    let bitValue = bitValuef bitSum s
    s |> Seq.filter (fun x -> x &&& mask > 0UL = bitValue)
    
let o2Filter = filter (fun bitsum s -> bitsum * 2UL >= uint64(Seq.length s)) //Partial application. Love it!
let co2Filter = filter (fun bitsum s -> bitsum * 2UL < uint64(Seq.length s))

let masks = [| for i in 0..11 -> 1UL <<< i|] |> Array.rev //work bitwise left to right

let rating filter = masks 
                    |> Seq.fold (fun s mask -> if Seq.length s = 1 then s 
                                                else filter mask s) lines
                    |> Seq.exactlyOne
                    |> int

let puzzle2 = rating co2Filter * rating o2Filter