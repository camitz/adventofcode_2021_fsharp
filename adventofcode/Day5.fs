module Day5

open System 

type Direction =
    | Horz
    | Vert
    | Diag //Puzzle 2 only


//Prepare the datastructures
let fileLines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day5coords.txt")

let order line =
    match line with
        | ((a,_),(c,_)) when a = c -> 
            match line with
                | ((_,b),(_,d)) when b > d -> (snd line, fst line)
                | _ -> line
        | ((a,_),(c,_)) when a > c -> (snd line, fst line)
        | _ -> line

let lines = fileLines 
            |> Seq.map (fun x -> x.Split (" -> ,".ToCharArray(),StringSplitOptions.RemoveEmptyEntries))
            |> Seq.map (fun x ->  Array.map (fun (y:string) -> Convert.ToInt32 y) x)
            |> Seq.map (fun [|a;b;c;d;|] -> order ((a,b),(c,d)))



let x1 line = fst (fst line)
let x2 line  = fst (snd line)
let y1 line  = snd (fst line)
let y2 line  = snd (snd line)

let direction line =
    match line with 
        | _ when x1 line = x2 line -> Vert
        | _ when y1 line = y2 line -> Horz
        | _ -> Diag //Puzzle 2 only
    
let yRange line =
    match [y1 line.. y2 line] with
    | [] -> List.rev [y2 line..y1 line]
    | _ -> [y1 line..y2 line]

let coveredPoints line =
    match direction line with 
        | Vert -> set(yRange line) |> Set.map (fun y -> (x1 line, y))
        | Horz -> set[x1 line..x2 line] |> Set.map (fun x -> (x, y1 line))
        | _ -> set (Seq.zip [x1 line..x2 line] (yRange line))


let multipleCoveredPoints ls = 
    ls
    |> Seq.map (fun line -> coveredPoints line)
    |> Seq.concat
    |> Seq.groupBy id
    |> Seq.choose (fun (k,v) -> match Seq.length v with        
                                         | x when x >= 2 -> Some k
                                         | _            -> None)

let puzzle1 = 
    lines 
    |> Seq.filter (fun x -> direction x <> Diag)
    |> multipleCoveredPoints
    |> Seq.length

let puzzle2 = 
    lines 
    |> multipleCoveredPoints
    |> Seq.toList
    |> Seq.length

