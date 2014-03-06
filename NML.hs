{-# LANGUAGE OverloadedStrings #-}
module NML (
NetworkObject(..),
Path,
GUID,
guid,

SingleType(..),
GroupType(..),
ServiceType(..),

NMLReader, runNMLReader, local,
NMLData(..), mkNMLData,

Cost
) where

import NML.RDFRep (NMLReader, runNMLReader, local, NMLData(..), mkNMLData, Cost)
import NML.Types
