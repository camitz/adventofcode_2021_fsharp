module Day4Puzzle1

open System 

let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day4boards.txt")

let draws = (Seq.head lines).Split ','

let boards = Seq.skip 2 lines 
            |> Seq.chunkBySize 6
            |> Seq.map (fun line -> Seq.take 5 line)
            |> Seq.map (fun board -> board 
                                    |> Seq.map (fun row -> row.Split(' ', StringSplitOptions.RemoveEmptyEntries))
                                    |> Seq.map (fun row -> row
                                                        |> Seq.map (fun number -> Convert.ToInt32(number.Trim()))
                                                        |> Seq.toArray)
                                    |> Seq.toArray)
            |> Seq.map (fun board -> Array2D.init 5 5 (fun i j -> board.[i].[j]))

let b = boards |> Seq.head
let puzzle1 = b.[1,*]