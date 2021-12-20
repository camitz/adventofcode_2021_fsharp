module Day10puzzle1

open System 

let openClose c =
    match c with
        | '(' -> Some(')')
        | '[' -> Some(']')
        | '{' -> Some('}')
        | '<' -> Some('>')
        | _ -> None

let score c =
    match c with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0

let peek a =
    List.head a

let push a c =
    (c::a)
    
let pop a =
    match a with 
        | [] -> []
        | head::tail -> tail


let subsystems = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day10subsystems.txt")
                |> Seq.map (fun s -> s.ToCharArray())

let rec parse stack remaining = 
    //printfn "%A %A" (String(Array.ofList remaining)) (String(Array.ofList stack))
    match remaining with
        | [] -> None
        | head::tail -> 
            match openClose head with
                | None -> 
                    match openClose (peek stack) with
                        | Some(c) when c = head -> parse (pop stack) tail
                        | _ -> Some(head)
                | Some(c) -> parse (push stack head) tail

let puzzle1 = 
    subsystems
    //|> Seq.take 1
    |> Seq.map Array.toList
    |> Seq.map (parse List.empty)
    |> Seq.choose id
    |> Seq.map score
    |> Seq.sum
    