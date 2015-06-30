-- | A weighted directed network represented as an 'forest', a map of maps.
module Manticore.Network where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

-- | A network maps some keys to other keys with an edge (the value 'v').
type Network k v = Map k (Map k v)

-- | Prints an undirected network.
showUNetwork :: (Show k0, Show k1, Ord k0, Ord k1) => Map k0 (Set k1) -> String
showUNetwork = Map.foldWithKey vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges e = let s = Set.foldr (\k acc -> show k ++ ", " ++ acc) "" e in
              take (length s - 2) s -- There must be a more elegant solution...

-- | Prints the network.
showNetwork :: (Show k, Show v, Ord k) => Network k v -> String
showNetwork = Map.foldWithKey vertices ""
  where
    vertices k v acc = show k ++ " -> " ++ edges v ++ "\n" ++ acc
    edges = Map.foldrWithKey (\k v acc -> "(" ++ show k ++ ", " ++ show v ++ "), " ++ acc) ""

-- | Second order lookup function.
lookup2 :: (Ord k0, Ord k1) => k0 -> k1 -> Map k0 (Map k1 v) -> Maybe v
lookup2 key0 key1 m = Map.lookup key0 m >>= Map.lookup key1

-- | Get the value from the edge between two vertices.
getVal :: (Ord k) => k -> k -> Network k v -> Maybe v
getVal = lookup2

-- | Tests for the presence of a vertex.
hasVertex :: (Ord k) => k -> Network k v -> Bool
hasVertex = Map.member

-- | Tests for the presence of a list of vertices.
hasVertices :: (Ord k) => [k] -> Network k v -> Bool
hasVertices vs m = all (`hasVertex` m) vs

-- | Tests for the presence of an edge.
hasEdge :: (Ord k) => (k, k) -> Network k v -> Bool
hasEdge (t, h) n =
  case Map.lookup t n of
    Nothing -> False
    Just m  -> Map.member h m

-- | Tests for the presense of a list of edges.
hasEdges :: (Ord k) => [(k, k)] -> Network k v -> Bool
hasEdges es n = all (`hasEdge` n) es

-- | Returns the outgoing edges for a given vertex.
outEdges :: (Ord k) => k -> Network k v -> Map k v
outEdges v n = fromMaybe Map.empty $ Map.lookup v n

-- | Returns the set of outgoing edges for a given vertex.
outEdgesSet :: (Ord k) => k -> Network k v -> Set k
outEdgesSet v n = Map.keysSet $ outEdges v n

-- | Returns the ingoing edges for a given vertex.
inEdges :: (Ord k) => k -> Network k v -> Map k v
inEdges v n = Map.foldWithKey addEdges Map.empty n
  where
    addEdges k _ a =
      case getVal k v n of
        Nothing -> a
        Just x  -> Map.insert k x a

-- | Returns the set of ingoing edges for a given vertex.
inEdgesSet :: (Ord k) => k -> Network k v -> Set k
inEdgesSet v n = Set.fold addKeys Set.empty $ Map.keysSet n
  where
    addKeys k a =
      case getVal k v n of
        Nothing -> a
        Just _  -> Set.insert k a

-- | Returns the set of vertices that can be reached from 'v0' (not
-- necessarily adjacent).
connectsTo :: (Ord k) => k -> Network k v -> Set k
connectsTo v0 n
  | Map.member v0 n = conn v0 $ Set.fromList [v0]
  | otherwise       = Set.empty
  where
    -- Recursive function to explore the network from 'v' given a set of
    -- visited ('vis') vertices.
    conn v vis =
      Set.fold (\k acc ->
        if Set.member k acc then acc
        else conn k $ Set.insert k acc)
      vis $ outEdgesSet v n

-- | Returns the set of vertices that cannot be reached from 'v'.
notConnected :: (Ord k) => k -> Network k v -> Set k
notConnected v n = Set.difference (Map.keysSet n) (connectsTo v n)

-- | Returns true if there is a path between the vertex and all other vertices
-- in the network.
connected :: (Ord k) => k -> Network k v -> Bool
connected v n = connectsTo v n == Map.keysSet n

-- | Returns true if there is a path between all pairs of vertices.
stronglyConnected :: (Ord k) => Network k v -> Bool
stronglyConnected n = all (`connected` n) $ Map.keys n
