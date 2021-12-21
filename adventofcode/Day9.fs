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
    if j < snd size - 1 && heatmap.[i,j+1] <= h then 
        None
    elif i < fst size - 1 && heatmap.[i+1,j] <= h then 
        None
    elif j > 0 && heatmap.[i,j-1] <= h then 
        None
    elif i > 0 && heatmap.[i-1,j] <= h then 
        None
    else
        Some(i,j)
        

let lowPoints = 
    heatmap
    |> Array2D.mapi (fun h i j -> lowPointHeat h i j)
    |> flatten
    |> Seq.choose id

let puzzle1 = 
    lowPoints
    |> Seq.sumBy (fun (i,j) -> heatmap.[i,j]+1)

let determineBasin p =
    let visited = Array2D.create (fst size) (snd size) false

    let rec crawl p =
        if fst p < 0 || snd p < 0  then
            0
        elif fst p >= fst size || snd p >= snd size  then
            0
        elif visited.[fst p, snd p] then
            0
        elif heatmap.[fst p, snd p] = 9 then
            0
        else 
            Array2D.set visited (fst p) (snd p) true
            1 +
            (crawl (fst p, snd p + 1)) +
            (crawl (fst p, snd p - 1)) +
            (crawl (fst p + 1, snd p)) +
            (crawl (fst p - 1, snd p)) 
    
    crawl p

let puzzle2 =
    lowPoints
    |> Seq.map determineBasin 
    |> Seq.sort
    |> Seq.rev
    |> Seq.take 3
    |> Seq.reduce (*)