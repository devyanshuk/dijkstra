### Dijkstra's algorithm implementation in Haskell

#### Running the Program
Install the [Haskell Platform](https://www.haskell.org/platform/) and in your terminal, type in the following:
```
$ ghci Main.hs
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/dev/.ghci
[1 of 3] Compiling Deps.Graph       ( Deps/Graph.hs, interpreted )
[2 of 3] Compiling Deps.Dijkstra    ( Deps/Dijkstra.hs, interpreted )
[3 of 3] Compiling Main             ( Main.hs, interpreted )
Ok, three modules loaded.
>
```

#### Sample Usage
```
> g <- fromFile "graph"
> g
Graph [("v1",[Neighbor {item = "v2", weight = 1.0},Neighbor {item = "v4", weight = 34.0}]),("v2",[Neighbor {item = "v4", weight = 2.0}]),("v4",[Neighbor {item = "v6", weight = 2.0}]),("v3",[Neighbor {item = "v7", weight = 2.0}])]
>
> displayShortestPathsFrom g "v1"
"v1" : "v1" Distance : 0.0
"v2" : "v1" -> "v2" Distance : 1.0
"v3" :  Distance : Infinity
"v4" : "v1" -> "v2" -> "v4" Distance : 3.0
"v6" : "v1" -> "v2" -> "v4" -> "v6" Distance : 5.0
"v7" :  Distance : Infinity
> 
> allEdges g
[(("v1","v2"),1.0),(("v1","v4"),34.0),(("v2","v4"),2.0),(("v4","v6"),2.0),(("v3","v7"),2.0)]
> 
> outgoingEdge g "v2"
[Neighbor {item = "v4", weight = 2.0}]
> 
> getPathTo "v6" $ dijkstra g "v1"
["v1","v2","v4","v6"]
```

#### Sample Usage II ( Console input )

```
> g <- fromConsole
Format : [vertex1] [vertex2] [weight]. Press Enter to stop giving input
a b 11
b c 2
a d 9
b d 2
>
> import Data.List(reverse)
>
> reverse $ getPathTo "d" $ dijkstra g "a"
["a","d"]
>
> displayShortestPathsFrom g "a"
"a" : "a" Distance : 0.0
"b" : "a" -> "b" Distance : 11.0
"c" : "a" -> "b" -> "c" Distance : 13.0
"d" : "a" -> "d" Distance : 9.0
> 
```