{-# LANGUAGE OverloadedStrings #-}
module NML.Query
(
subjectOf, predicateOf, objectOf,

queryS, queryO,

getAllOfType, getByNameOfType, getByName,
getNodesAndPorts, getNetworkObjects
) where

import Control.Monad (liftM)
import Data.RDF.Query (subjectOf, predicateOf, objectOf)
import Data.Text (Text)

import NML
import NML.RDFRep

type Encoding = Text

-- FIXME needs testing
---- | get all NetworkObjects
--getAll :: NetworkObject -> NMLReader [NetworkObject]
--getAll = baseQueryWrap (id, toRDF, id) toNetworkObject
--            Subject Nothing (Just ("rdf:type" :: Text)) Nothing

-- | get all NetworkObjects of the same type
getAllOfType :: NetworkObject -> NMLReader [NetworkObject]
getAllOfType = baseQueryWrap (id, toRDF, id) toNetworkObject
            Subject Nothing (Just ("rdf:type" :: Text)) . Just . toRDFType

-- | get all NetworkObjects of a certain type by their name
getByNameOfType :: Text -> NetworkObject -> NMLReader [NetworkObject]
getByNameOfType s = baseQueryWrap (toRDF, toRDF, id) toNetworkObject
            Subject (Just s) (Just ("rdf:type" :: Text)) . Just . toRDFType

-- | get all NetworkObjects of a certain type by their name
getByName :: Text -> NMLReader [NetworkObject]
getByName s = baseQueryWrap (toRDF, toRDF, id) toNetworkObject
            Subject (Just s) (Just ("rdf:type" :: Text)) Nothing

-- TODO: refactor to NMLReader
getLayers :: NetworkObject -- ^ Topology
	-> [Encoding]
getLayers = error "getLayers is undefined"

-- | get all NetworkObjects
-- FIXME should this also include Services?
getNetworkObjects :: NMLReader [NetworkObject]
getNetworkObjects = liftM concat $ mapM getAllOfType [Single Node "",
                                                      Single Port "",
                                                      Single Link ""]

getNodesAndPorts :: NMLReader [NetworkObject]
getNodesAndPorts =  getAllOfType (Single Node "") >>= \nodes ->
                    getAllOfType (Single Port "") >>= \ports ->
                    return $ nodes ++ ports

-- | query and return subject
queryS :: NetworkObject -- ^ object
	-> Text 	-- ^ predicate
	-> NMLReader [NetworkObject]
queryS obj pred = queryWrap Subject Nothing (Just pred) (Just obj)

-- | query and return object
queryO :: NetworkObject -- ^ subject
	-> Text	-- ^ predicate
	-> NMLReader [NetworkObject]
queryO subj pred = queryWrap Object (Just subj) (Just pred) Nothing
