module Reduction.Segmentation where
import NML (Path)

connectingPaths :: [Path] -> [Path]
connectingPaths = id -- Haskell's identity function

-- | validateSegmentation returns True when ther are no connecting paths,
-- and False otherwise.
validateSegmentation :: [Path] -> Bool
validateSegmentation ps = null ps
