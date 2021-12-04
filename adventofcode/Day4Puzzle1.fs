module Day4Puzzle1

open System 

let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day4boards.txt")

let draws = (Seq.head lines).Split ',' |> Seq.map Convert.ToInt32

let boards = Seq.skip 2 lines 
            |> Seq.chunkBySize 6
            |> Seq.map (fun line -> Seq.take 5 line)
            |> Seq.map (fun board -> board 
                                    |> Seq.map (fun row -> row.Split(' ', StringSplitOptions.RemoveEmptyEntries))
                                    |> Seq.map (fun row -> row
                                                        |> Seq.map Convert.ToInt32
                                                        |> Seq.toArray)
                                    |> Seq.toArray)
            |> Seq.map (fun board -> Array2D.init 5 5 (fun i j -> board.[i].[j]))

let b = boards |> Seq.head

let hasBingo (board : int [,]) draws =
     ([0..4] |> Seq.exists (fun r -> (Set.intersect (set draws) (set board.[r,*])).Count = 5),
      [0..4] |> Seq.exists (fun c -> (Set.intersect (set draws) (set board.[*,c])).Count = 5))  
        <> (false,false)
        

let puzzle1 = 
    printfn "%A" (Seq.toList (Seq.take 14 draws))
    hasBingo b (Seq.take 14 draws)