{-# LANGUAGE OverloadedStrings #-}
module NML.Types (
NetworkObject(..),
Path,
GUID,
guid,

SingleType(..),
GroupType(..),
ServiceType(..),
) where
import Data.Text (Text)

data NetworkObject 	= Single SingleType GUID
			| Group GroupType GUID
			| Service ServiceType GUID
	deriving (Show, Eq, Ord)

type Path = [NetworkObject]

type GUID = Text

data SingleType = Node | Port | Link
	deriving (Show, Eq, Ord)
data GroupType = Topology | PortGroup | LinkGroup | BidirectionalPort | BidirectionalLink
	deriving (Show, Eq, Ord)
data ServiceType = Switch | Adaptation | Deadaptation
	deriving (Show, Eq, Ord)

guid :: NetworkObject -> GUID
guid (Single  _ g) = g
guid (Group   _ g) = g
guid (Service _ g) = g
