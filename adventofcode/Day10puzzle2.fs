module Day10puzzle2

open System 

let openClose c =
    match c with
        | '(' -> Some(')')
        | '[' -> Some(']')
        | '{' -> Some('}')
        | '<' -> Some('>')
        | _ -> None

let rec score (t:uint64) stack =
    let scoreNextWith a = score (5UL * t + a)
    match stack with
        | [] -> t
        | head::tail ->
            match head with
            | '(' -> scoreNextWith 1UL tail
            | '[' -> scoreNextWith 2UL tail
            | '{' -> scoreNextWith 3UL tail
            | '<' -> scoreNextWith 4UL tail
            | _ -> 0UL

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
        | [] -> Some(stack)
        | head::tail -> 
            match openClose head with
                | None -> 
                    match openClose (peek stack) with
                        | Some(c) when c = head -> parse (pop stack) tail
                        | _ -> None
                | Some(c) -> parse (push stack head) tail

let puzzle2 = 
    subsystems
    //|> Seq.take 1
    |> Seq.map Array.toList
    |> Seq.map (parse List.empty)
    |> Seq.choose id
    |> Seq.toList
    |> List.map (score 0UL)
    |> List.sort
    |> (fun x -> 
            let p = (List.length x) / 2
            List.item p x
       )
