module Day11

open System 

let flatten a = 
     seq { for x in [0..(Array2D.length1 a) - 1] do 
            for y in [0..(Array2D.length2 a) - 1] do 
                yield a.[x, y] }

let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day11energies.txt")

let energies0 = 
            lines 
            |> Seq.map (fun s -> s.ToCharArray() |> Array.map (fun c -> (int c)-(int '0')))
            |> Seq.toArray

let size = (energies0.Length, energies0.[0].Length)

let energies = Array2D.init (fst size) (snd size) (fun i j ->  energies0.[i].[j])
    
let step (e, count) =
    //First
    let e = e |> Array2D.map (fun x -> x + 1)

    let adjacentCoords i j  =
         seq { for x in [-1..1] do 
                for y in [-1..1] do 
                    yield (i+x, j+y) }
         |> Seq.filter (fun x -> 
                            match x with 
                            | _ when x=(i,j) -> false
                            | (-1,_) | (_,-1) -> false
                            | (ii,jj) when ii=fst size || jj=snd size -> false
                            | _ -> true
                        )

    let flashElementFromAdjacent (e:int[,]) (f:bool[,]) i j x =    
        x + ((adjacentCoords i j)
                |> Seq.filter (fun (x,y) -> not f.[x,y])
                |> Seq.map (fun (x,y) -> e.[x,y])
                |> Seq.sumBy (fun a -> if a >= 10 then 1 else 0))
            

    let flashElements e f = 
        e |> Array2D.mapi (flashElementFromAdjacent e f)

    let setFlashed e (f:bool[,]) = 
        e |> Array2D.mapi (fun i j x -> x >= 10 || f.[i,j])


    //Then
    let rec subStep (e, f) =
        let e1 = flashElements e f
        let f1 = setFlashed e f

        if f1=f then
            (e1,f1)
        else
            subStep (e1,f1)
    
    let (e, f) = subStep (e, Array2D.create (fst size) (snd size) false)

    //Finally
    (e |> Array2D.mapi (fun i j x -> if f.[i,j] then 0 else x),
            count +
            ((flatten f)
            |> Seq.filter id
            |> Seq.length))

let stepN n =
    [0..n-1]
    |> List.map (fun _ -> step)
    |> List.reduce (>>)

let puzzle1 =
    snd (stepN 100 (energies, 0))
