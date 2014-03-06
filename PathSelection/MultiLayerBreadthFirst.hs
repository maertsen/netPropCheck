{-# LANGUAGE TupleSections #-}
module PathSelection.MultiLayerBreadthFirst (
multiLayerBreadthFirst
)where
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Map.Strict as Map
import qualified Data.Heap as Heap
import Data.Set (Set)
import qualified Data.Set as Set

import NML
import NML.Relations (expandForward, getParentNode)
import NML.Attributes
import NML.Query (getNetworkObjects)
import Util

-- | 'PathObject' used in MinHeap, using distance for comparison.
data PathObject = PathObject    { visitedLinks      :: [NetworkObject]              -- ^ sequence of visited edges
                                , labelStack        :: [Label]                      -- ^ current technology stack
                                , technologyStack   :: [AdaptationFunction]         -- ^ current technology stack
                                , distance          :: !Int                          -- ^ distance covered
                                , loopSet           :: Set (NetworkObject, Maybe AdaptationFunction, Maybe Label)
                                } deriving (Eq, Show)
instance Ord PathObject where
    PathObject{distance=d1} `compare` PathObject{distance=d2} = d1 `compare` d2

currentNode :: PathObject -> NetworkObject
currentNode PathObject{visitedLinks=ns} = head ns

previousNode :: PathObject -> Maybe NetworkObject
previousNode PathObject{visitedLinks=ns} = safeHead =<< safeTail ns

type Source = NetworkObject 
type Destination = NetworkObject

-- | wrapper for 'multiLayerBreadthFirst'' returning [Path]
multiLayerBreadthFirst :: Source -> Destination -> NMLReader [Path]
multiLayerBreadthFirst = (liftM $ map (tail . reverse . visitedLinks)) .: multiLayerBreadthFirst'

-- | multiLayerBreadthFirst algorithm (path selection in G_l)
--   as published in 
--   "Path selection in mult-layer networks" by Kuipers, Dijkstra, 2008
multiLayerBreadthFirst' :: Source -> Destination -> NMLReader [PathObject]
multiLayerBreadthFirst' src dst = go [] (Heap.singleton (PathObject [src] [] [] 0 ls)) src dst -- enter main loop with empty result and minHeap
    where
        ls = Set.singleton (src, Nothing, Nothing)

        -- | loop until either heap is exhausted or we arrive at our
        --   destination with an empty technologystack
        go :: [PathObject] -> Heap.MinHeap PathObject -> Source -> Destination -> NMLReader [PathObject]
        go result heap src dst = case Heap.view heap of
                Nothing             ->  return result -- Heap exhausted
                Just (minPO, heap') ->  (checkDestination (currentNode minPO) dst) >>= \arrived ->
                                        if  (arrived && (null $ technologyStack minPO))
                                        -- found a possible path, add it to the result list
                                        then go (minPO:result) heap' src dst
                                        else do let cur = currentNode minPO
                                                let prev = previousNode minPO
                                                -- use topology to determine next hops
                                                next <- expandForward prev cur
                                                -- foreach: extendpath
                                                heap'' <- foldr (f minPO) (return heap') next
                                                -- recurse using updated heap
                                                go result heap'' src dst

        f :: PathObject -> NetworkObject -> NMLReader (Heap.MinHeap PathObject) -> NMLReader (Heap.MinHeap PathObject)
        f p n h = extendPath p n >>= \extension ->
                case extension of
                    Nothing     -> h
                    Just p'     -> liftM2 Heap.insert (return p') h

-- used to be:
-- extendPath :: PathObject -> (Link, Node) -> Maybe PathObject
extendPath :: PathObject -> NetworkObject -> NMLReader (Maybe PathObject)
extendPath p n = runMaybeT $ do
    let visitedLinks' = n:(visitedLinks p)

    let distance'     = distance p + 1 -- this can be swapped for a more realistic distance metric

    -- perform adaptation / de-adaptation check
    adaptationFunction <- lift $ adaptationFunction n
    let tStack = technologyStack p
    technologyStack' <- maybeZero $ case n of
                (Service Adaptation _)    -> Just $ maybe (error $ "Adaptation without adaptationFunction property: "++show n)
                                             (:tStack) adaptationFunction
                -- ^ grow the technology stack with the technology in this Adaptation
                (Service Deadaptation _)  -> maybe (error $ "Deadaptation without adaptationFunction property: " ++ show n)
                                             (\t -> if ((not $ null tStack) && t == head tStack)
                -- pop the technology stack with the technology in this Adaptation
                                             then Just $ tail tStack
                -- or short circuit Maybe monad to reject Path due to incompatibility
                                             else Nothing) adaptationFunction
                -- if this is not an adaptation, the stack does not change
                _                         -> Just tStack

    -- perform label check
    currentLabel <- lift $ label $ currentNode p
    nextLabel <- lift $ label n
    let lStack = labelStack p

    -- we add the label for the current node to the stack if the extension is an adaptation
    labelStack' <- maybeZero $ case n of
                (Service Adaptation _)    -> Just $ maybe (error $ "Adaptation from Port without label: "++(show $ currentNode p)) (:lStack) currentLabel
                -- grow the label stack with the label in this Adaptation
                _                         -> Just lStack

    -- we do an deadaptation check if the current node is an deadaptation
    -- by checking whether the extension's label is already on the label stack
    labelStack'' <- maybeZero $ case currentNode p of
                (Service Deadaptation _)  -> maybe (error $ "Deadaptation to Port without label: "++show n)
                                             (\l -> if ((not $ null labelStack') && l == head labelStack')
                -- pop the label stack with the label in this Adaptation
                                             then Just $ tail labelStack'
                -- or short circuit Maybe monad to reject Path due to incompatibility
                                             else Nothing) nextLabel
                -- if this is not an adaptation, the stack does not change
                _                         -> Just labelStack'
    
    -- check whether the triple (NetworkObject, Technology, Label) is already present in our loopSet,
    -- if so, we just looped back to our own path and we will not continue.
    let loopTriple = (n, adaptationFunction , nextLabel)
    loopSet' <- maybeZero $ if loopTriple `Set.member` loopSet p
                            then Nothing
                            else Just $ Set.insert loopTriple $ loopSet p

    -- return new PathObject, extended by one hop
    maybeZero $ Just $ PathObject visitedLinks' labelStack'' technologyStack' distance' loopSet'

checkDestination    :: NetworkObject    -- ^ current location
                    -> NetworkObject    -- ^ destination
                    -> NMLReader Bool             -- ^ whether or not the destination has been reached
checkDestination loc dest@(Single Node _)   = liftM (maybe False (== dest)) $ getParentNode loc  -- if the destination is a Node, check whether the current location has that node as a 'parent'
checkDestination loc dest@(Group _ _)       = error "checkDestination is undefined for Group's"
checkDestination loc dest@(Service _ _)     = error "checkDestination is undefined for Services"
checkDestination loc dest                   = return $ loc == dest
