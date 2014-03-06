{-# LANGUAGE OverloadedStrings #-}
module NML.Attributes (
AdaptationFunction, adaptationFunction,
Encoding, encoding, mkEncoding,
noReturnTraffic,
Label, label,
) where
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text

import NML.Query
import NML.RDFRep
import NML.Types
import Util

newtype AdaptationFunction = AdaptationFunction { unpackAdaptationFunction :: GUID } deriving (Eq, Ord, Show)

-- | get the 'AdaptationFunction' for a 'NetworkObject' if it has one
adaptationFunction :: NetworkObject -> NMLReader (Maybe AdaptationFunction)
adaptationFunction n = case n of
            (Service Deadaptation _)    -> q n
            (Service Adaptation _)      -> q n
            _                           -> return Nothing
    where
        q = queryAttrib "nml:adaptationFunction" AdaptationFunction

newtype Encoding = Encoding { unpackEncoding :: GUID } deriving (Eq, Show)

-- | get the 'Encoding' for a 'NetworkObject' if it has one
encoding :: NetworkObject -> NMLReader (Maybe Encoding)
encoding n = case n of
            (Single Link _)                 -> q n
            (Single Port _)                 -> q n
            (Group LinkGroup _)             -> q n
            (Group PortGroup _)             -> q n
            (Service Switch _)              -> q n
            _                               -> return Nothing
    where
        q = queryAttrib "nml:encoding" Encoding

mkEncoding :: GUID -> Encoding
mkEncoding = Encoding

-- | get the 'noReturnTraffic' for a 'NetworkObject' if it has one or
--   False if it doesn't
noReturnTraffic :: NetworkObject -> NMLReader Bool
noReturnTraffic n = case n of
            (Single Link _)                 -> liftM (fromMaybe False) (q n)
            _                               -> return False
    where
        q = queryAttrib "nml:noReturnTraffic" (=="true")

type LabelType = GUID
type LabelValue = Text
-- | Label is implemented as an attribute here, to simplify implementation it
-- is important to note that NML specifies it as a class on equal footing with
-- e.g. NetworkObject
newtype Label = Label { unpackLabel :: (GUID, LabelType, LabelValue) } deriving (Ord, Show)

instance Eq Label where
    a == b = let (_, ta, va) = unpackLabel a
                 (_, tb, vb) = unpackLabel b
             in (ta == tb) && (va == vb)

label :: NetworkObject -> NMLReader (Maybe Label)
label n = case n of
            (Single Link _)                 -> q n
            (Single Port _)                 -> q n
            _                               -> return Nothing
    where
        q n = runMaybeT $
                labelGuid n >>= \guid ->
                labelType guid  >>= \type' ->
                labelValue guid >>= \value ->
                maybeZero $ Just $ Label $ (guid, type', value)

        labelGuid :: NetworkObject -> MaybeT NMLReader GUID
        labelGuid n = MaybeT $ liftM listToMaybe $ baseQueryWrap (id,toRDF,id)
            (return . unpackUNode) Object (Just $ toRDF n) (Just "nml:hasLabel" :: Maybe Text) Nothing

        labelType :: GUID -> MaybeT NMLReader LabelType
        labelType = MaybeT . queryAttrib "nml:type" id

        labelValue :: GUID -> MaybeT NMLReader LabelValue
        labelValue = MaybeT . queryAttrib "nml:value" id

-- | utility function to simplify attribute queries
queryAttrib :: (RDFRep r) => Text -> (Text -> a) -> r -> NMLReader (Maybe a)
queryAttrib attrib wrap n = liftM listToMaybe $
    baseQueryWrap (id,toRDF,id) pack Object (Just $ toRDF n)
                                            (Just attrib)
                                             Nothing
    where
        pack = return . wrap . unpackUNode
