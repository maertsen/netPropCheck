module Reduction.TimeToRecovery (
worstCaseRecoveryCost, validateTimeToRecovery
) where
import Data.List (maximumBy, minimumBy)

import NML

type RecoveryCost = NetworkObject -> Cost


-- | Compute the 'NetworkObject' with the worst 'Cost' among a list of
--   'Path's.
worstCaseRecoveryCost :: RecoveryCost -> [Path] -> (NetworkObject, Cost)
worstCaseRecoveryCost f [] = error "no path supplied"
worstCaseRecoveryCost f paths = (pair . min . max) paths
	where
		-- find the bottleneck for each path
		max :: [[NetworkObject]] -> [NetworkObject]
		max nss = map (maximumBy (recoveryCompare f)) nss

		-- find the optimum amongst the bottlenecks
		min :: [NetworkObject] -> NetworkObject
		min ns = minimumBy (recoveryCompare f) ns

		pair :: NetworkObject -> (NetworkObject, Cost)
		pair n = (n, f n)
		
-- | Specialised comparison function for 'NetworkObject's in terms of
--   'RecoveryCost'
recoveryCompare :: RecoveryCost -> NetworkObject -> NetworkObject -> Ordering
recoveryCompare f a b = f a `compare` f b

-- | validateTimeToRecovery returns True when there is a 'Path' within the
--   worst case recovery time given, False otherwise.
validateTimeToRecovery :: Int -> (NetworkObject, Cost) -> Bool
validateTimeToRecovery n (_,c) = n >= c
