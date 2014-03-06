{-# LANGUAGE OverloadedStrings #-}
module Poc where

import Control.Monad
import Data.Maybe
import Data.Text (isInfixOf, unpack)
import qualified Data.Map as Map

import NML
import NML.Attributes (Encoding, encoding, mkEncoding)
import NML.Relations (getParentNode)
import NML.Query (getNetworkObjects, getAllOfType)

type Hours = Int

getRecoveryTime' :: NMLReader (NetworkObject -> Hours)
getRecoveryTime' = do
                allNO <- getNetworkObjects
                allRT <- mapM getRecoveryTime allNO
                let timemap = Map.fromList $ zip allNO allRT

                return $ \n -> Map.findWithDefault 0 n timemap

getRecoveryTime :: NetworkObject -> NMLReader Hours
getRecoveryTime n@(Single Node g) | isRouter = return $ 5 * 24
                                  | isDistributionSwitch = return $ 3 * 24
                                  | isAccessSwitch = return 4
                                  | isAccessPoint = return 4
                                  | isHandHole = return $ 3*24
                                  | isPocSystem = return 1
                                  | otherwise = error $ "no recoverytime defined for " ++ unpack g

    where
        isRouter = ".routing" `isInfixOf` g
        isDistributionSwitch = any (`isInfixOf` g) ["sw1-sein-campus.civ"
                                                    ,"sw1-t-campus.civ"
                                                    ,"sw1-cam.civ"
                                                    ,"sw1-iih.civ"
                                                    ,"sw1-box-a.civ"
                                                    ,"sw1-sky-a.civ"
                                                    ,"sw1-cal.civ"
                                                    ,"sw1-mat.civ"
                                                    ,"sw1-wbw.civ"
                                                    ,"sw1-sein-snt.civ"
                                                    ,"sw1-t-halb.civ"]
        isAccessSwitch = ":sw" `isInfixOf` g && ".civ" `isInfixOf` g
        isAccessPoint = ".wlan" `isInfixOf` g
        isHandHole = ":glas" `isInfixOf` g
        isPocSystem = ":reviresco" `isInfixOf` g
getRecoveryTime n@(Single Port _) = maybe (return 0) getRecoveryTime =<< getParentNode n
getRecoveryTime n@(Single Link g) | inGlasRing = return $ 3*24
                                  | otherwise  = return 1
    where
        inGlasRing = "glas" `isInfixOf` g

getSpeedCriterium :: Int -> NMLReader (NetworkObject -> Bool)
getSpeedCriterium minSpeed = do
                allNO <- getAllOfType (Single Port "")
                allSpeeds <- mapM getSpeed allNO
                let speedmap = Map.fromList $ map (\(a,b) -> (a,fromJust b)) $ filter (isJust.snd) $ zip allNO allSpeeds

                return $ \n -> case n of
                            (Single Port _) -> minSpeed <= Map.findWithDefault (minSpeed+1) n speedmap
                            _               -> True

type Mbit = Int

getSpeed :: NetworkObject -> NMLReader (Maybe Mbit)
getSpeed n = case n of
        (Single Port _)  -> getSpeed' n
        (Single Link _)  -> getSpeed' n
        _                -> return Nothing
    where
        getSpeed' n = encoding n >>= \enc ->
            return $ enc >>= encodingToLinkSpeed

encodingToLinkSpeed :: Encoding -> Maybe Mbit
encodingToLinkSpeed e   | e `elem` fastE    = Just 100
                        | e `elem` gigE     = Just 1000
                        | e `elem` tenGigE  = Just 10000
                        | otherwise         = Nothing
    where
        fastE   = map mkEncoding ["10BASE-T/100BASE-TX"]
        gigE    = map mkEncoding ["1000BASE-GEN", "1000BASE-LX", "1000BASE-SX", "100/1000BASE-T"]
        tenGigE = map mkEncoding ["10GIGBASE-GEN"]

nodeToModel :: NetworkObject -> Maybe String
nodeToModel n = Map.lookup (guid n) $ Map.fromList
    [("urn:ogf:network:utwente.nl:2014:sw1-cal49.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-box-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-mat30.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw387.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-bio.civ", "2610-24"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-f.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal7.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wot.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw383.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw377.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cad.civ", "2610-24"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam49.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw393.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-bos.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw1-logica.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw2-t-halb.civ", "5308XL"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-sein-campus.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-fc.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-k.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw401.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-t-campus.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-sleutel.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-d.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal5.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-e.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal10.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-c.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-keet.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal13.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw389.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-h.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal9.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw381.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal53.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-j.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw3-mat30.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-holz.civ", "5304XL"),
    ("urn:ogf:network:utwente.nl:2014:sw1-sky-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam41.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw399.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw2-iih-l.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-l.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal55.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-g.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal50.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam29.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw2-mat30.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-sein-kgb.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw385.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-box-b.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal38.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam55.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-mat75.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam35.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal11.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-linde.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw1-pav.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-zwembad.civ", "2610-24/12PWR"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal1.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw397.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-ukast.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-holz-b.civ", "2610-24"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-i.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam69.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-sc-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw3-htf-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw2-cal50.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-mat.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-icsc.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-sein-snt.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-mat73.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-hdlab-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw395.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-t-halb.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cam59.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw2-htf-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-t-kgb.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-schuur.civ", "2610-24"),
    ("urn:ogf:network:utwente.nl:2014:sw2-cam29.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-b.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal3.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-htf-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-wbw379.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-cal20.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-iih-a.civ", "5406zl"),
    ("urn:ogf:network:utwente.nl:2014:sw1-toren.civ", "5406zl")]
