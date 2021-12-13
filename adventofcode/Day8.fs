module Day8

open System 

//Prepare the datastructures
let lines = IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day8readings.txt")

let input = fst
let output = snd
let single = Seq.exactlyOne 

let intersect a b = 
    Set.intersect (set(a)) (set(b))

let diff a b = 
    Set.difference (set(a)) (set(b))

let readings = lines 
                    |> Seq.map (fun l -> l.Split(" |".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
                    |> Seq.map (fun rs -> (rs.[0..9], rs.[10..]))

let puzzle1 = 
    readings 
    |> Seq.map (fun rs -> output rs |> 
                            Seq.sumBy (fun o -> if o.Length <=4 or o.Length = 7 then 1 else 0))
    |> Seq.sum


let segmentCount =
    Seq.collect id >>
    Seq.groupBy id >>
    Seq.map (fun (s,n) -> (s, Seq.length n))

let deduceConnections inputs =
    let segemenstWithCount m =
        segmentCount inputs
        |> Seq.choose (fun(s,n)-> if n = m then Some s else None)

    let segmentsOfDigitsOfLength m =
        inputs 
            |> Seq.filter (fun (i:string) -> i.Length = m)
            |> Seq.map (fun d -> d.ToCharArray())
            |> Seq.collect id

    //abfcadg
    Seq.zip (Seq.rev("ebfcadg".ToCharArray())) 
        (List.fold (fun r f -> (f r)::r) [] 
            ([(fun r -> segemenstWithCount 4 |> single);
     
            (fun r -> segemenstWithCount 6 |> single);
     
            (fun r -> segemenstWithCount 9 |> single);
     
            (fun r -> intersect  
                            (segemenstWithCount 8)
                            (segmentsOfDigitsOfLength 2)
                        |> single);

            (fun r -> diff 
                            (segmentsOfDigitsOfLength 3)
                            r
                        |> single);
        

            (fun r -> diff 
                            (segmentsOfDigitsOfLength 4)
                            r
                        |> single);
    
            (fun r -> diff 
                            (segmentsOfDigitsOfLength 7)
                            r
                        |> single);
            ])
        )
        |> Seq.map (fun (k, v) -> (v,k))
        |> Map.ofSeq


let rewire input = 
    let mapping = deduceConnections input
 
    input
        |> Seq.map (fun x -> x.ToCharArray() 
                                |> Array.map (fun c -> mapping.[c])
                                |> (fun s -> System.String(s))
                    )

let targetDigits = [|("abcefg");"cf";"acdeg";"acdfg";"bcdf";"abdfg";"abdefg";"acf";"abcdefg";"abcdfg"|]

let interpretDigit (digit : string) = 
    let chars = set(digit.ToCharArray())

    targetDigits
        |> Array.findIndex (fun d -> chars = set(d.ToCharArray()))
    

let puzzle = 
    input (readings |> Seq.head)
    |> segmentCount
    |> Seq.toList


let puzzle2 = 
    //input (readings |> Seq.head)
    targetDigits
    |> rewire 
    |> Seq.map interpretDigit
    //|> digitsOfLength 5
    |> Seq.toList

