module Day3Puzzle1

open System 

let readLines filePath = IO.File.ReadLines filePath 
                        |> Seq.map (fun n -> Convert.ToUInt64(n, 2))

let lines = readLines (__SOURCE_DIRECTORY__ + @"\day3diagnostics.txt")

let nLines = uint64 (Seq.length lines) 

let sumWithMask ns mask = ns 
                            |> Seq.map (fun n -> mask &&& n)
                            |> Seq.sum 

let masks = [| for i in 0..11 -> 1UL <<< i|] |> Array.rev //work bitwise left to right

let gammaBitSeq = masks 
                |> Seq.map (fun mask -> sumWithMask lines mask / mask) //sum bits and divide by mask to produce the count in that position
                |> Seq.map (fun bitSum -> bitSum > nLines / 2UL) //determine if count is more than half the number of lines
                |> Seq.map (function |true -> 1UL |false -> 0UL) 
 
let gamma = Seq.zip gammaBitSeq masks //zip together bit position mask and bit sequence to poroduce final value
            |> Seq.sumBy (fun x -> fst x * snd x) 

let epsilon = 0b111111111111UL ^^^ gamma //epsilon is bit flipped gamma 

let puzzle1 = gamma * epsilon