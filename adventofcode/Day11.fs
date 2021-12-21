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
    
let mutable count = 0

let step e0 =
    //First
    let add1if f e = 
        e
        |> Array2D.map (fun x -> if f x then x + 1 else x)

    let e1 = add1if (fun _ -> true) e0
    //printfn "e1:\n%A" e1


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

    //printfn "adj:%A" (Seq.toList (adjacent 3 0))

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
    let subStep (e, f) =
        let e11 = flashElements e f
        //printfn "e11:\n%A" e1

        let f1 = setFlashed e f
        //printfn "f:\n%A" f1

        (e11,f1)
    
    let flash10 =
        [0..20]
        |> List.map (fun _ -> subStep)
        |> List.reduce (>>)


    let (e2, f1) = flash10 (e1, Array2D.create (fst size) (snd size) false)

    //printfn "e2:%A" e2
    //Finally
    let e3 = e2
            |> Array2D.mapi (fun i j x -> if f1.[i,j] then 0 else x)

    printfn "e3:\n%A" e3

    count <- count +
                ((flatten f1)
                |> Seq.filter id
                |> Seq.length)

    e3

let step100 =
    [0..99]
    |> List.map (fun _ -> step)
    |> List.reduce (>>)

let puzzle1 =
    let t = step100 energies
    count