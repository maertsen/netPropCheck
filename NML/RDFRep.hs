{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving, TupleSections #-}
module NML.RDFRep (

RDFRep, toRDF,
toRDFType,
wrap,
toNetworkObject,
NMLReader, runNMLReader, NMLData(..), mkNMLData,
verbose, verboseM, verboseM', verboseM'M, paths,

local,
Cost, getCostFunction,
getControlCriterium,
getMaximumCliqueSize,

unpackUNode, Selector(..),
queryWrap, queryWrapRDF, baseQueryWrap
) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.Writer.Lazy
import Data.Functor.Identity
import Data.RDF.MGraph
import Data.RDF.Types
import qualified Data.RDF.Types as RDF (Node(..), unode)
import Data.RDF.Query (subjectOf, objectOf)
import Data.Text (Text)

import NML.Types

class RDFRep rep where
	toRDF :: rep -> RDF.Node

instance RDFRep NetworkObject where
	toRDF = RDF.unode . guid

instance RDFRep Text where
	toRDF = RDF.unode

toRDFType :: NetworkObject -> RDF.Node
toRDFType (Single Node             _) = UNode "nml:Node"
toRDFType (Single Port             _) = UNode "nml:Port"
toRDFType (Single Link             _) = UNode "nml:Link"
toRDFType (Group Topology          _) = UNode "nml:Topology"
toRDFType (Group PortGroup         _) = UNode "nml:PortGroup"
toRDFType (Group LinkGroup         _) = UNode "nml:LinkGroup"
toRDFType (Group BidirectionalPort _) = UNode "nml:BidirectionalPort"
toRDFType (Group BidirectionalLink _) = UNode "nml:BidirectionalLink"
toRDFType (Service Switch          _) = UNode "nml:SwitchingService"
toRDFType (Service Adaptation      _) = UNode "nml:AdaptationService"
toRDFType (Service Deadaptation    _) = UNode "nml:DeadaptationService"

wrap :: Text -> Text -> NetworkObject
wrap "nml:Node" 		= Single Node
wrap "nml:Port" 		= Single Port
wrap "nml:Link" 		= Single Link
wrap "nml:Topology" 		= Group Topology
wrap "nml:PortGroup" 		= Group PortGroup
wrap "nml:LinkGroup" 		= Group LinkGroup
wrap "nml:BidirectionalPort" 	= Group BidirectionalPort
wrap "nml:BidirectionalLink" 	= Group BidirectionalLink
wrap "nml:SwitchingService" 	= Service Switch
wrap "nml:AdaptationService" 	= Service Adaptation
wrap "nml:DeadaptationService" 	= Service Deadaptation

data NMLData = NMLData  { nml :: MGraph
                        , costFunction :: NetworkObject -> Int
                        , controlCriterion :: NetworkObject -> Bool}

type Description = String
type Detail = String
type PropCheckLogEntry = (Description, Detail)
type PropCheckLog = ([Path], [PropCheckLogEntry])

mkNMLData = NMLData undefined undefined undefined

-- type NMLReader = Reader (MGraph, Int)
newtype NMLReader a = NMLReader { unwrapNMLReader :: ReaderT NMLData (WriterT PropCheckLog Identity) a}
    deriving (Functor, Applicative, Monad)
-- TODO: derive MonadTrans instance

runNMLReader :: NMLReader a -> NMLData -> (a, PropCheckLog)
runNMLReader n r = runIdentity $ runWriterT $ runReaderT (unwrapNMLReader n) r

getRDF :: NMLReader MGraph
getRDF = NMLReader $ asks nml

verbose :: Description -> Detail -> NMLReader ()
verbose descr entry = NMLReader $ lift $ writer ((), ([], [(descr, entry)]))

verboseM :: (Show a) => String -> NMLReader a -> NMLReader a
verboseM descr m = m >>= (verbose descr) . show >> m

verboseM' :: (Show b) => String -> (a -> b) -> NMLReader a -> NMLReader a
verboseM' descr f m = m >>= (verbose descr) . show . f >> m

verboseM'M :: (Show b) => String -> (a -> NMLReader b) -> NMLReader a -> NMLReader a
verboseM'M descr f m = m >>= (liftM (verbose descr) . liftM show . f) >> m

paths :: [Path] -> NMLReader [Path]
paths ps = NMLReader $ lift $ writer (ps, (ps, []))

local :: (NMLData -> NMLData) -> NMLReader a -> NMLReader a
local f = NMLReader . Control.Monad.Trans.Reader.local f . unwrapNMLReader

type Cost = Int
-- | Alias for a 'Cost' lookup function
getCostFunction :: NMLReader (NetworkObject -> Cost)
getCostFunction = NMLReader $ asks costFunction

getControlCriterium :: NMLReader (NetworkObject -> Bool)
getControlCriterium = NMLReader $ asks controlCriterion

getMaximumCliqueSize :: NMLReader (Maybe Int)
getMaximumCliqueSize = return $ Nothing

toNetworkObject :: RDF.Node 			-- ^ node to be identified
		-> NMLReader NetworkObject	-- ^ resulting NetworkObject wrapping node
toNetworkObject n@(UNode g) = liftM2 wrap rtype (return g)
	where
		p = toRDF ("rdf:type" :: Text)
		-- this causes a runtime error when there is more than one "rdf:type"
		rtype =	getRDF >>= \r ->
			let [Triple _ _ (UNode t)] = query r (Just n) (Just p) Nothing
			in return t

unpackUNode :: Node -> Text
unpackUNode (UNode s) = s
unpackUNode (LNode (PlainL t)) = t
unpackUNode (LNode (PlainLL t _)) = t
unpackUNode (LNode (TypedL t _)) = t
unpackUNode _ = error "unpackUNode is undefined"

data Selector = Subject | Object

queryWrap       :: Selector			-- ^ create NetworkObject based on (subject or object)
		-> Maybe NetworkObject		-- ^ RDF subject
		-> Maybe Text			-- ^ RDF predicate
		-> Maybe NetworkObject		-- ^ RDF object
		-> NMLReader [NetworkObject]    -- ^ list of results
queryWrap = baseQueryWrap (toRDF, toRDF, toRDF) toNetworkObject

queryWrapText   :: Selector			-- ^ create NetworkObject based on (subject or object)
		-> Maybe Text			-- ^ RDF subject
		-> Maybe Text			-- ^ RDF predicate
		-> Maybe Text			-- ^ RDF object
		-> NMLReader [Text]	        -- ^ list of results
queryWrapText = baseQueryWrap (toRDF, toRDF, toRDF) $ return . unpackUNode

queryWrapRDF    :: Selector			-- ^ create NetworkObject based on (subject or object)
		-> Maybe Node			-- ^ RDF subject
		-> Maybe Node			-- ^ RDF predicate
		-> Maybe Node			-- ^ RDF object
		-> NMLReader [Node]	        -- ^ list of results
queryWrapRDF = baseQueryWrap (id,id,id) return

baseQueryWrap   :: (s -> RDF.Node, p -> RDF.Node, o -> RDF.Node) -- ^ pre query wrapper
                -> (RDF.Node -> NMLReader r)    -- ^ post query wrapper
                -> Selector		        -- ^ create NetworkObject based on
                                                --   (subject or object)
		-> Maybe s			-- ^ RDF subject
		-> Maybe p			-- ^ RDF predicate
		-> Maybe o			-- ^ RDF object
		-> NMLReader [r]	        -- ^ list of results
baseQueryWrap (preS,preP,preO) post sel s p o = case sel of 	Subject -> selector subjectOf
					                        Object 	-> selector objectOf
    where
        selector sel' = triples >>= (mapM (post . sel'))

        triples :: NMLReader [Triple]
        triples = liftM queryWith getRDF
        queryWith = \r -> query r (fmap preS s) (fmap preP p) (fmap preO o)
