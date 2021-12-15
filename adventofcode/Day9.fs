module Day9

open System 

let flatten a = 
     seq { for x in [0..(Array2D.length1 a) - 1] do 
            for y in [0..(Array2D.length2 a) - 1] do 
                yield a.[x, y] }

let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day9heatmap.txt")

let heatmap0 = 
            lines 
            |> Seq.map (fun s -> s.ToCharArray() |> Array.map (fun c -> (int c)-(int '0')))
            |> Seq.toArray

let size = (heatmap0.Length, heatmap0.[0].Length)

let heatmap = Array2D.init (fst size) (snd size) (fun i j ->  heatmap0.[i].[j])

let lowPointHeat i j h =
    //printfn "%A of %A: %d" (i,j) size h
    if j < snd size - 1 && heatmap.[i,j+1] <= h then 
        None
    elif i < fst size - 1 && heatmap.[i+1,j] <= h then 
        None
    elif j > 0 && heatmap.[i,j-1] <= h then 
        None
    elif i > 0 && heatmap.[i-1,j] <= h then 
        None
    else
        Some(h+1)
        

let lowPoints = 
    heatmap
    |> Array2D.mapi (fun h i j -> lowPointHeat h i j)

let puzzle1 = 
    lowPoints
    |> flatten
    |> Seq.choose id
    |> Seq.sum