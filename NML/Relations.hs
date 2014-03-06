{-# LANGUAGE OverloadedStrings #-}
module NML.Relations where

import Control.Monad
import Data.Maybe (listToMaybe)
import Prelude hiding (exp)

import NML
import NML.Query (queryO, queryS)
import NML.Attributes (noReturnTraffic)
import Util

-- | expand yields the next hops for a 'NetworkObject'
expand :: NetworkObject -> NMLReader [NetworkObject]
expand n@(Single Node _) = exp [hasAdaptationService, hasOutboundPort, isAlias] n
expand n@(Single Port _) = exp [hasDeadaptationService, isSource, inverseProvidesPort] n
expand n@(Single Link _) = exp [isSerialCompoundLink, inverseIsSink] n
expand n@(Group PortGroup _) = error "expand is undefined for PortGroup"
expand n@(Group LinkGroup _) = error "expand is undefined for LinkGroup"
expand n@(Service Adaptation _) = exp [inverseHasService] n
expand n@(Service Deadaptation _) = exp [providesPort] n
expand _ = error "expand is undefined"

-- | expandForward yields the next hops for a 'NetworkObject',
--   filtering the way back
expandForward 	:: Maybe NetworkObject      -- ^ previous NetworkObject
		-> NetworkObject            -- ^ current NetworkObject (to be expanded)
		-> NMLReader [NetworkObject]
expandForward Nothing     n = expand n
expandForward (Just prev) n = case n of
                                l@(Single Link _)   -> expansion >>= filterM (returnTraffic l)
                                _                   -> expansion
    where
        expansion = (liftM $ filter (/=prev)) $ expand n

        returnTraffic :: NetworkObject -> NetworkObject -> NMLReader Bool
        returnTraffic link port = do
                broadcastMedium <- noReturnTraffic link
                returnPort <- isReverseDirection prev port
                return $ not $ broadcastMedium && returnPort
            where

-- | retrieve the `parent` Node for a Network Object
--   this is only defined for those objects that have a parent-like relation in
--   the NML standard
getParentNode   :: NetworkObject
                -> NMLReader (Maybe NetworkObject)
getParentNode n@(Single Node _)     = return $ Just n
getParentNode n@(Single Port _)     = liftM listToMaybe $ exp [inverseHasOutboundPort, inverseHasInboundPort] n
getParentNode n@(Single Link _)     = return $ Nothing -- a Link is not 'owned'
getParentNode n@(Service Switch _)  = liftM listToMaybe $ exp [inverseHasService] n
getParentNode n@(Service _ _)       = return $ Nothing -- (de)adaptationservices are 'owned' by a Port, not by a Node
getParentNode n                     = return $ Nothing -- all other Network Objects don't have a clear parent node

-- | hasTopology ('NetworkObject' to Topology)
hasTopology = qO "nml:hasTopology"

-- | hasNode ('NetworkObject' to Node)
hasNode = qO "nml:hasNode"

-- | implementedBy (Node to Node)
implementedBy = qO "nml:implementedBy"

-- | hasService ('NetworkObject' to Service)
hasService = qO "nml:hasService"

-- | hasAdaptationService ('NetworkObject' to Service)
hasAdaptationService = (liftM $ filter adaptation) . qO "nml:hasService"
    where
        adaptation (Service Adaptation _) = True
        adaptation _                        = False

-- | hasDeadaptationService ('NetworkObject' to Service)
hasDeadaptationService = (liftM $ filter deadaptation) . qO "nml:hasService"
    where
        deadaptation (Service Deadaptation _) = True
        deadaptation _                        = False

-- | inverseHasService (Service to 'NetworkObject')
inverseHasService = qS "nml:hasService"

-- | hasPort ('NetworkObject' to Port/Port Group) or (PortGroup to Port)
hasPort = qO "nml:hasPort"

-- | inverseHasPort (Port/Port Group to 'NetworkObject') or (Port to PortGroup)
inverseHasPort = qS "nml:hasPort"

-- | hasLink ('NetworkObject' to Link/Link Group) or (LinkGroup to Link)
hasLink = qO "nml:hasLink"

-- | providesPort (Service to Port/PortGroup)
providesPort = qO "nml:providesPort"

-- | inverseProvidesPort(Port/PortGroup to (de)adaptationservice)
inverseProvidesPort = qS "nml:providesPort"

-- | providesLink (Service to Link/LinkGroup)
providesLink = qO "nml:providesLink"

-- | hasInboundPort ('NetworkObject' to Port/PortGroup)
hasInboundPort = qO "nml:hasInboundPort"

-- | inverseHasInboundPort (Port/PortGroup to 'NetworkObject')
inverseHasInboundPort = qS "nml:hasInboundPort"

-- | hasOutboundPort ('NetworkObject' to Port/PortGroup)
hasOutboundPort = qO "nml:hasOutboundPort"

-- | inverseHasOutboundPort (Port/PortGroup to 'NetworkObject')
inverseHasOutboundPort = qS "nml:hasOutboundPort"

-- | isSource (Port to Link or PortGroup to LinkGroup)
isSource = qO "nml:isSource"

-- | isSink (Port to Link or PortGroup to LinkGroup)
isSink = qO "nml:isSink"

-- | inverseIsSink (Link to Port)
inverseIsSink = qS "nml:isSink"

-- | isSerialCompoundLink (Link to Ordered List)
isSerialCompoundLink = qO "nml:isSerialCompoundLink"

-- | isAlias ('NetworkObject' to 'NetworkObject')
isAlias = qO "nml:isAlias"

-- | next (List Item to List Item) (RDF)
next = qO "nml:next"

inverseHasLink = qS "nml:hasLink"

-- | is reverse paired with n in a Bidirectional group?
isReverseDirection  :: NetworkObject    -- ^ the 'NetworkObject' to reverse
                    -> NetworkObject    -- ^ the reverse to be compared with
                    -> NMLReader Bool
isReverseDirection object reverse = case object of
        (Single Port _) -> isReverse hasPort inverseHasPort portFilter object
        (Single Link _) -> isReverse hasLink inverseHasLink linkFilter object
    where
        isReverse   :: (NetworkObject -> NMLReader [NetworkObject]) -- ^ groupToMember
                    -> (NetworkObject -> NMLReader [NetworkObject]) -- ^ memberToGroup
                    -> (NetworkObject -> Bool)                      -- ^ groupType filter
                    -> NetworkObject
                    -> NMLReader Bool
        isReverse sel revSel f = return . (reverse `elem`) <=< (liftM concat . mapM sel) . (filter f) <=< revSel
        portFilter x = case x of {(Group BidirectionalPort _) -> True; _ -> False}
        linkFilter x = case x of {(Group BidirectionalLink _) -> True; _ -> False}

--
-- Helper functions (not exported)
--

exp :: [NetworkObject -> NMLReader [NetworkObject]] -> NetworkObject -> NMLReader [NetworkObject]
exp x = liftM concat . sequence . ap x . return

qO = flip queryO
qS = flip queryS
