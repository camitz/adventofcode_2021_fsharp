module Day2Puzzle1

open System 

type Position = {
        Horz:int
        Depth:int
    }

type Control =
    | Up
    | Down
    | Forward

type Command = Command of control:Control * n:int

let parseControl s = match s with
    | "up" -> Up
    | "down" -> Down
    | "forward" -> Forward

let issueCommand p1 c = match c with
    | Command(Up, n) -> {Horz = p1.Horz; Depth = p1.Depth - n}
    | Command(Down, n) -> {Horz = p1.Horz; Depth = p1.Depth + n}
    | Command(Forward, n) -> {Horz = p1.Horz + n; Depth = p1.Depth}

let parse a = match a  with
| [| c; n |] -> parseControl c, Int32.Parse(n)
| _ -> Up, 0

let readLines filePath = IO.File.ReadLines(filePath) 
                        |> Seq.map (fun line -> line.Split ' ')
                        |> Seq.map parse
                        |> Seq.map (fun x -> Command(fst x, snd x))

let lines = readLines (__SOURCE_DIRECTORY__ + @"\day2course.txt")
 
let p0 = {Horz = 0; Depth = 0}

let navigate = lines 
             |> Seq.fold (fun p1 c -> issueCommand p1 c) p0

let puzzle1 = navigate.Depth * navigate.Horz

