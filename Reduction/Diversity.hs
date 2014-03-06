module Reduction.Diversity (distinctPaths, validateDiversity) where
import Data.List (delete, intersect, maximumBy)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map.Lazy as Map

import NML (Path)
import Util.Clique (getCliques)

-- | 'distinctPaths' returns zero or more groups of the highest # of distinct
--   paths. Every group consists of pairwise distinct paths. An optional maximum
--   diversity can be supplied, which is used to switch to (polynomial) k-clique
--   algorithms.
distinctPaths :: Maybe Int -> [Path] -> [[Path]]
distinctPaths _ [] = error "no path supplied"
distinctPaths max ps = (getCliques max . filter distinct . pairs) ps
	where
		distinct :: (Path, Path) -> Bool
		distinct (a,b) = null (intersect a b)

                pairs :: [Path] -> [(Path, Path)]
		pairs [] = []
		pairs (p:ps) = [(p,b)|b<-ps] ++ pairs ps

validateDiversity :: Int -> [[Path]] -> Bool
validateDiversity n pss = ((>=n) . length . head) pss
