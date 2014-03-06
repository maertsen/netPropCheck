module Reduction.Control (Criterium, unsafePaths, validateControl) where
import NML (NetworkObject, Path)

type Criterium = NetworkObject -> Bool

-- | inControl abstracts a 'Criterium' over a 'Path'
--   It yield True only if the entire 'Path' is under control
inControl :: Criterium -> Path -> Bool
inControl f p = all f p

-- | unsafePaths returns a list of 'Path' not satisfying the 'Criterium'
unsafePaths :: Criterium -> [Path] -> [Path]
unsafePaths f ps = filter (not . inControl f) ps

-- | validateControl return True if all Path's are under control, False
--   otherwise.
validateControl :: [Path] -> Bool
validateControl ps = null ps
