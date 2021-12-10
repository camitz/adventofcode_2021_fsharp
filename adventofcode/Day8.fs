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

    [
     yield ("e", segemenstWithCount 4 |> single)
     
     let b = segemenstWithCount 6 |> single
     yield ("b", b)
     
     let f =  segemenstWithCount 9 |> single
     yield ("f", f)
     
     let c = intersect 
                    (segemenstWithCount 8)
                    (segmentsOfDigitsOfLength 2)
                |> single
     yield ("c", c)

     yield ("a", diff
                        (segmentsOfDigitsOfLength 3)
                        [|c;f|]
                    |> single
           )

     yield ("d", diff
                        (segmentsOfDigitsOfLength 4)
                        [|b;c;f|]
                    |> single
           )
    ]

let puzzle = 
    input (readings |> Seq.head)
    |> segmentCount
    |> Seq.toList

let testString = [|"abcefg";"cf";"acdeg";"acdfg";"bcdf";"abdfg";"abdefg";"acf";"abcdefg";"abcdfg"|]

let puzzle2 = 
    //input (readings |> Seq.head)
    testString
    |> deduceConnections 
    //|> digitsOfLength 5
    //|> Seq.toList

