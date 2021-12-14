module Day9

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day9manual.txt")
            |> Seq.map (fun l -> l.Split(", =".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))

type Cart = X | Y
    
type Point = int * int
type Fold = Cart * int

let points = 
    lines 
    |> Seq.choose(fun p -> match p with 
                            | [|x;y|] -> Some(Point(Int32.Parse x, Int32.Parse y))
                            | _ -> None 
                    )

let foldAlong f p =
    match f with
        | (X,a) when fst p > a -> Point(a + a - fst p, snd p)
        | (Y,a) when snd p > a -> Point(fst p, a + a - snd p)
        | _ -> p

let foldPointsAlong (f:Fold) (ps:Point seq) =    
    let fa = foldAlong f

    ps 
    |> Seq.map (fun p -> fa p)

let folds  = 
    lines 
    |> Seq.choose(fun p -> match p with 
                            | [|_;_;a;b|] when a="x" -> Some(Fold(X, Int32.Parse b))
                            | [|_;_;a;b|] when a="y" -> Some(Fold(Y, Int32.Parse b))
                            | _ -> None 
                    )

let puzzle1 = 
    foldPointsAlong (Seq.head(folds)) points
    |> set
    |> Set.count

let foldComposition =
    folds
    |> Seq.map (fun f -> foldPointsAlong f) 
    |> Seq.reduce (>>) 

let puzzle2 = 
    foldComposition points
    |> set
    |> Set.count
