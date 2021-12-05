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

let drawSequence =
        draws 
        |> Seq.toList 
        |> List.scan (fun acc draw -> acc @ [draw]) List.empty<int>
        |> List.tail

//Thx: https://stackoverflow.com/a/12564172/168390
let takeUntil predicate s = 
    seq { yield! Seq.takeWhile predicate s
          yield! s |> Seq.skipWhile predicate |> Seq.truncate 1 }

//Thx: http://www.fssnip.net/oq/title/Array2D-to-one-dimension
let flatten board = 
     seq { for x in [0..(Array2D.length1 board) - 1] do 
            for y in [0..(Array2D.length2 board) - 1] do 
                yield board.[x, y] }

let winningDraw = 
    drawSequence
    |> takeUntil (fun draws -> 
        boards |> Seq.map (fun board -> hasBingo board draws)
                |> Seq.forall (fun r -> not r))
    |> Seq.last
        
let winningBoard = 
    boards 
    |> Seq.find (fun board -> hasBingo board winningDraw)
    
let puzzle1 = 
            (Set.difference (set(flatten winningBoard)) (set winningDraw)
            |> Seq.sum) * (Seq.last winningDraw)