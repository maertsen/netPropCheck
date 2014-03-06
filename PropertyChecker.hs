module PropertyChecker (

Compute, ComputeM,
Validate,
computeProperty, verifyProperty,

selectPaths, stripPath,

segmentation, control, diversity, timeToRecovery,
validateSegmentation, validateControl, validateDiversity, validateTimeToRecovery

) where

import Control.Monad
import Control.Monad.Loops (dropWhileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (tell)
import Data.Maybe (isNothing)

import NML
import NML.RDFRep
import NML.Relations (getParentNode)
import PathSelection.MultiLayerBreadthFirst
import Reduction.Segmentation
import Reduction.Control
import Reduction.Diversity
import Reduction.TimeToRecovery
import Util

-- | a computation on top of path selection
type Compute a = [Path] -> a
-- | a computation on top of path selection,
--   using knowledge of the topology ('NMLReader')
type ComputeM a = [Path] -> NMLReader a
-- | a validation of a computation
type Validate a = a -> Bool

-- | Computation of a property for a source and destination,
--   using knowledge of the topology ('NMLReader')
computeProperty :: (Show a) => ComputeM a -> NetworkObject -> NetworkObject -> NMLReader a
computeProperty p = (p <=<) . selectPaths

-- | Validation of a property for a source and destination,
--   using knowledge of the topology ('NMLReader')
validateProperty :: Validate a -> a -> NMLReader Bool
validateProperty = (return .)

-- | Verification of a property for a source and destination,
--   using knowledge of the topology ('NMLReader')
--   verification = computation + validation
verifyProperty :: (Show a) => ComputeM a -> Validate a -> NetworkObject -> NetworkObject -> NMLReader Bool
verifyProperty c v n1 n2 = validateProperty v =<< computeProperty c n1 n2

selectPaths :: NetworkObject -> NetworkObject -> NMLReader [Path]
selectPaths = (paths =<<) .: multiLayerBreadthFirst

stripPath :: NetworkObject -> NetworkObject -> [Path] -> NMLReader [Path]
stripPath s d = stripSourcePrefixes s >=> stripDestinationPostfixes d

stripSourcePrefixes :: NetworkObject -> [Path] -> NMLReader [Path]
stripSourcePrefixes s = mapM removePrefix
    where
        removePrefix :: Path -> NMLReader Path
        removePrefix p = getParentNode s >>= \sourceParent ->
                        dropWhileM (\n ->   getParentNode n >>= \parent -> return $
                                            isNothing parent || parent == sourceParent) p

stripDestinationPostfixes :: NetworkObject -> [Path] -> NMLReader [Path]
stripDestinationPostfixes d ps = return . map reverse =<< stripSourcePrefixes d (map reverse ps)

segmentation :: ComputeM [Path]
segmentation = liftM connectingPaths . return

control :: ComputeM [Path]
control = (verboseM'M "unsafe paths" $ liftM2 annotateUnsafePaths getControlCriterium . return) . liftM2 unsafePaths getControlCriterium . return

annotateUnsafePaths :: Criterium -> [Path] -> [[Either NetworkObject NetworkObject]]
annotateUnsafePaths f = map (map (\n -> if not $ f n then Left n else Right n))

diversity :: ComputeM [[Path]]
diversity = (verboseM' "number of distinct paths" (length . head)) . liftM2 distinctPaths getMaximumCliqueSize . return

timeToRecovery :: ComputeM (NetworkObject, Cost)
timeToRecovery = (verboseM "sample NetworkObject with worst recovery cost") . liftM2 worstCaseRecoveryCost getCostFunction . return
