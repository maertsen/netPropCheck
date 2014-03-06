{-# LANGUAGE OverloadedStrings #-}
module PropertyChecker.TUI where
import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import Data.RDF
import Data.RDF.MGraph
import Data.Text (pack)
import qualified Data.Text.IO as Text (readFile)
import Data.Traversable (traverse)
import Options.Applicative
import Text.RDF.RDF4H.XmlParser
import Text.Read (readMaybe)
import System.Exit (exitFailure)

import NML
import NML.Relations (getParentNode)
import NML.Query (getNodesAndPorts, getByName)
import Poc
import PropertyChecker
import Util

data TUI = TUI
    { file :: String
    , property :: Maybe String
    , source :: Maybe String
    , destination :: Maybe String
    , paths :: Bool
    , verbose :: Bool
    }

properties = [  "segmentation",
                "control",
                "diversity",
                "timetorecovery"]

tuiParser :: Parser TUI
tuiParser =  TUI 
        <$> argument str
            (metavar "FILE")
        <*> optional (argument str
            (  metavar "PROP"
            <> help "property to be verified, omit for list" ))
        <*> optional (strOption
            (  long "source"
            <> short 's'
            <> metavar "SOURCE"
            <> help "source NetworkObject for property verification, omit for list" ))
        <*> optional (strOption
            (  long "destination"
            <> short 'd'
            <> metavar "DESTINATION"
            <> help "destination NetworkObject for property verification, omit for list" ))
        <*> switch
            (  long "paths"
            <> short 'p'
            <> help "show results of path selection step " )
        <*> switch
            (  long "verbose"
            <> short 'v'
            <> help "show information gathered while checking properties" )

tui :: TUI -> IO ()
tui (TUI file prop src dst showPaths verbose) = do
    rdf <- fmap fromEither $ parseFile (XmlParser Nothing Nothing) file
    let reader f = runNMLReader f (mkNMLData {nml=rdf})
    let freader = fst . reader
    -- we show Nodes and Ports as options
    let choices = unlines $ map (show . guid) $ freader $ getNodesAndPorts

    let collapse = (listToMaybe =<<)

    -- find src in list of all network objects (don't restrict to just nodes and ports)
    let srcNode = collapse $ freader $ traverse getByName $ pack <$> src
    let dstNode = collapse $ freader $ traverse getByName $ pack <$> dst
    let validProp = maybe False (`elem` properties) prop

    when (isNothing srcNode) $ putStrLn "Possible sources are:" >> putStrLn choices
    when (isNothing dstNode) $ putStrLn "Possible destinations are:" >> putStrLn choices
    unless validProp $ putStrLn "Possible properties are:" >> putStrLn (unlines properties)

    when (isNothing srcNode || isNothing dstNode || not validProp) exitFailure

    -- this is safe, because of the exit above
    let process (Just p) (Just s) (Just d) = liftM reader $ selector p s d

    (output, (paths, log)) <- process prop srcNode dstNode

    let summarizePaths = if verbose  then id
                                     else deDup . mapMaybe (freader . getParentNode)

    when showPaths $ putStrLn $ unlines $ map (unlines . map show . summarizePaths) $ paths
    when verbose $ putStrLn $ unlines $ map show $ log

    putStrLn $ show $ output

main :: IO ()
main = execParser opts >>= tui
  where
    opts = info (helper <*> tuiParser)
      ( fullDesc
     <> progDesc "End to end property checking in NML descriptions"
     <> header "netPropCheck - a network property checker" )

selector :: String -> NetworkObject -> NetworkObject -> IO (NMLReader Bool)
selector "segmentation"   s d = return $ verifyProperty segmentation validateSegmentation s d
selector "control"        s d = getInt "Please enter the required minimum speed" >>= \minSpeed ->
                                  return $ getSpeedCriterium minSpeed >>= \f ->
                                  local (\nmldata -> nmldata{controlCriterion=f}) $
                                  verifyProperty control validateControl s d
selector "diversity"      s d = getInt "Please enter the required path diversity as an integer" >>= \reqDiversity ->
                                  return $ selectPaths s d >>= stripPath s d >>= diversity >>= return . (validateDiversity reqDiversity)
selector "timetorecovery" s d = getInt "Please enter the required time to recovery as an integer" >>= \reqTTR ->
                                    return $ getRecoveryTime' >>= \f ->
                                    local (\nmldata -> nmldata{costFunction=f}) $
                                    verifyProperty timeToRecovery (validateTimeToRecovery reqTTR) s d

--
-- Utility functions
--

deDup :: Eq a => [a] -> [a]
deDup []        = []
deDup (x:[])    = x:[]
deDup (x:y:xs)  = if x==y
                  then deDup (y:xs)
                  else (x:(deDup (y:xs)))

-- FIXME: automatically get minimum speed by using source link
--getMinSpeed :: NetworkObject -> NMLReader (IO Int)
--getMinSpeed n = getSpeed n >>= maybe
--                (liftIO $ putStrLn "Please use a Port as source, with a specified encoding" >> exitFailure)
--                (\a -> putStrLn "" >> return a)

getInt :: String -> IO Int
getInt str = do putStrLn str
                reqDiversity <- getLine
                case readMaybe reqDiversity of
                    Just n -> return n
                    Nothing-> getInt str
