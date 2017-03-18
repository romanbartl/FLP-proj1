{-# LANGUAGE RecordWildCards #-}

module Main
    (main)
where

import System.IO
import System.Environment
import Control.Applicative
import System.Directory
--import Data.List

import Types.BKG
import BKGParser

main :: IO ()
main = do  
   conf <- parseArgs <$> getArgs      --getArfs:: IO[String]
   case conf of 
      Right Config{..} -> handleInBKG Config{..}
      Left e -> print e

handleInBKG :: Config -> IO ()
handleInBKG Config{..} = do 
   -- cmp <- doesFileExist inGrammar    
   -- print cmp
    g <- case noInput of  
      True -> parseBKG <$> hGetContents stdin 
      otherwise -> parseBKG <$> readFile inGrammar 
    
    case g of
          Right Grammar{..} -> print g 
          Left e -> print e
 
    case pAction of
      PrintState -> print pAction
      FirstStep ->  print pAction
      SecondStep -> print pAction
    --return ()
    --where
    --parseInput = do 
    --  parseBKG <$> hGetContents stdin 

parseArgs :: [String] -> Either String Config
parseArgs [] = Left $ "Not enough arguments."
parseArgs [i] 
    | i == "-i" = Right $ Config PrintState True ""
    | i == "-1" = Right $ Config FirstStep True ""
    | i == "-2" = Right $ Config SecondStep True ""
    | otherwise = Left $ "Nezname argumenty"
parseArgs [i,n] 
    | i == "-i" = Right $ Config PrintState False n
    | i == "-1" = Right $ Config FirstStep False n
    | i == "-2" = Right $ Config SecondStep False n
    | otherwise = Left $ "Unknown arguments"
parseArgs _ = Left $ "Too many arguments."
