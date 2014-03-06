module NML.GraphRep ( 
NetworkObject(..), 
Path,

nodes, 
links, 
) where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree (Gr)
import Data.Text (Text)

import NML

-- DEPRECATED
--newtype Node = MkNode Graph.Node deriving (Eq)
--newtype Link = MkLink (Graph.LEdge LinkType) deriving (Eq, Ord)
--newtype Graph = MkGraph (Gr () LinkType)
--newtype Path = MkPath (Gr () LinkType)

nodes :: Path -> [NetworkObject]
nodes p = [ n | n@(Single Node _) <- p]

links :: Path -> [NetworkObject]
links p = [ l | l@(Single Link _) <- p]
