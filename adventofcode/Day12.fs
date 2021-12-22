module Day12

open System 


//Thanks: http://www.fssnip.net/av/title/NinetyNine-F-Problems-Problems-80-89-Graphs
type 'a Edge = 'a * 'a

type 'a Graph = 'a list * 'a Edge list

let g = (['b';'c';'d';'f';'g';'h';'k'],[('b','c');('b','f');('c','f');('f','k');('g','h')])

type 'a Node = 'a * 'a list

type 'a AdjacencyGraph = 'a Node list

let ga = [('b',['c'; 'f']); ('c',['b'; 'f']); ('d',[]); ('f',['b'; 'c'; 'k']); 
                                                    ('g',['h']); ('h',['g']); ('k',['f'])]

let bigCave (s:string) =
    s.ToUpper() = s

let graph2AdjacencyGraph ((ns, es) : 'a Graph) : 'a AdjacencyGraph =
    let nodeMap = ns |> List.map(fun n -> n, []) |> Map.ofList
    (nodeMap,es)
    ||> List.fold(fun map (a,b) -> map |> Map.add a (b::map.[a]) |> Map.add b (a::map.[b]))
    |> Map.toList
    
let adjacencyGraph2Graph (ns : 'a AdjacencyGraph) : 'a Graph=
    let sort ((a,b) as e) = if a > b then (b, a) else e
    let nodes = ns |> List.map fst
    let edges = (Set.empty, ns)
                ||> List.fold(fun set (a,ns) -> (set, ns) ||> List.fold(fun s b -> s |> Set.add (sort (a,b))) )
                |> Set.toSeq
                |> Seq.sort
                |> Seq.toList
    (nodes, edges)

let paths start finish (g : 'a AdjacencyGraph) allowSingle =
    let map = g |> Map.ofList
    let rec loop route single visited = [
        let current = List.head route
        if current = finish then
            yield List.rev route
        else
            for next in map.[current] do
                if next <> "start" then
                    if bigCave next || visited |> Set.contains next |> not  then
                        yield! loop (next::route) single (Set.add next visited)
                    if not (bigCave next) && not single && allowSingle then
                        yield! loop (next::route) true (Set.add next visited)
    ]
    loop [start] false <| Set.singleton start


//Prepare the datastructures
let lines = 
    IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\day12caves.txt") 
    |> Seq.map (fun x -> x.Split ("-".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun [|x;y|] -> (x,y))
    |> Seq.toList

let caveSystem =
    (
        (
            [for (a,y) in lines do
                yield! [a;y]]
            |> Set.ofList
            |> Set.toList
        ), lines
    )

let puzzle1 =
    paths "start" "end" (graph2AdjacencyGraph caveSystem) false
    |> List.length

let puzzle2 =
    paths "start" "end" (graph2AdjacencyGraph caveSystem) true
    |> Set.ofList //No idea why they're not distinct
    |> Set.toList
    |> List.sort
    |> List.length