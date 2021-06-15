module Deps.Dijkstra where

import Deps.Graph
import qualified Data.Set as Set

type ShortestPath a = [a]

dijkstra :: Graph a
            -> a {- source vertex -}
            -> a {- destination vertex -}
            -> Maybe(Weight, ShortestPath a)
            
dijkstra _ _ _ = Nothing