module Util.Parallel where

import Util
import Util.Parallel.Granularity

import GHC.Conc (numCapabilities)

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans

import qualified Control.Monad.Par as PM hiding (runParIO)
import qualified Control.Monad.Par.IO as PM

import qualified Control.Parallel as PP
import qualified Control.Parallel.Strategies as PP

-- TODO: Needs to works with Traversal

divideListByCore :: [a] -> [[a]]
divideListByCore list = makeGranList numCapabilities list
divideListByCoreMore :: [a] -> Int -> [[a]]
divideListByCoreMore list n = makeGranList (numCapabilities*n) list

divideListByCoreS :: [a] -> [[a]]
divideListByCoreS list = makeGranListSimple numCapabilities list
divideListByCoreMoreS :: [a] -> Int -> [[a]]
divideListByCoreMoreS list n = makeGranListSimple (numCapabilities*n) list

pMapBuffer f =
  PP.withStrategy (PP.parBuffer 100 PP.rdeepseq)
  . map f

pMap'       f list = PP.parMap PP.rpar f list
pMapC'      f list = reduceList       $ PP.parMap PP.rpar (map f) $ divideListByCore list
pMapCs'     f list = reduceListSimple $ PP.parMap PP.rpar (map f) $ divideListByCoreS list
pMapCn' n   f list = reduceList       $ PP.parMap PP.rpar (map f) $ divideListByCoreMore list n
pMapCns' n  f list = reduceListSimple $ PP.parMap PP.rpar (map f) $ divideListByCoreMoreS list n

pMap''      f list = PM.runPar $ PM.parMap f list
pMapC''     f list = reduceList       $ PM.runPar $ PM.parMap (map f) $ divideListByCore list
pMapCs''    f list = reduceListSimple $ PM.runPar $ PM.parMap (map f) $ divideListByCoreS list
pMapCn'' n  f list = reduceList       $ PM.runPar $ PM.parMap (map f) $ divideListByCoreMore list n
pMapCns'' n f list = reduceListSimple $ PM.runPar $ PM.parMap (map f) $ divideListByCoreMoreS list n

pMapIO     f list = PM.runParIO $ PM.parMapM (liftIO . return . f) list
-- FIXME: I'm not sure why this have problem
--pMapCIO    f list = reduceList <$> PM.runParIO $ PM.parMapM (liftIO . return . (map f)) $ divideListByCore list
--pMapCIOn n f list = reduceList <$> PM.runParIO $ PM.parMapM (liftIO . return . (map f)) $ divideListByCoreMore list n
pMapCIO    f list = do
  gList <- PM.runParIO $ PM.parMapM (liftIO . return . (map f)) $ divideListByCore list
  return $ reduceList gList
pMapCIOs   f list = do
  gList <- PM.runParIO $ PM.parMapM (liftIO . return . (map f)) $ divideListByCoreS list
  return $ reduceListSimple gList
pMapCIOn n f list = do
  gList <- PM.runParIO $ PM.parMapM (liftIO . return . (map f)) $ divideListByCoreMore list n
  return $ reduceList gList
pMapCIOns n f list = do
  gList <- PM.runParIO $ PM.parMapM (liftIO . return . (map f)) $ divideListByCoreMoreS list n
  return $ reduceListSimple gList
