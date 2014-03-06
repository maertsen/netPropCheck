module Util.Clique where
import qualified Data.Algorithm.MaximalCliques as MC (getMaximalCliques)
import qualified Data.Map.Lazy as Map

import NML (Path)
import Util

-- | Get the maximum cliques, possibly limited by an upper bound
--getMaximumCliques :: Maybe Int -> Map.Map Path [Path] -> [[Path]]
getCliques :: Maybe Int -> [(Path, Path)] -> [[Path]]
getCliques max = findCliques . group . expand
	where
		findCliques = case max of
						Nothing -> getMaximumCliques 
						Just n -> getKCliques n

		-- expand (a,b) to [(a,b),(b,a)]
		expand :: [(Path, Path)] -> [(Path, [Path])]
		expand [] = []
		expand ((a,b):ps) = (a,[b]) : (b, [a]) : expand ps

		-- map associating a path with all other distinct paths
		group :: [(Path, [Path])] -> Map.Map Path [Path]
		group = Map.fromListWith (++)

-- use the Bron-Kerbosch algorithm to find the maximal cliques
-- A clique is fully connected mesh of nodes within a graph, a maximal
-- clique is the largest of such cliques (modulo equally sized cliques)
getMaximumCliques = maximaByLength . maximal
	where
		maximal :: Map.Map Path [Path] -> [[Path]]
		maximal m = MC.getMaximalCliques (isNeighbour m) (Map.keys m)
		isNeighbour m a b = elem b $ Map.findWithDefault [] a m

-- | get k-clique
-- use [Vassilevska 2009] or better to get all k-sized cliques in
-- $O(n^k/(\epsilon \log n)^{k-1})$ (currently unimplemented)
getKCliques max = error "getKCliques is undefined"
