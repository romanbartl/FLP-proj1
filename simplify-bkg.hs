{-# LANGUAGE RecordWildCards #-}

module Main
    (main)
where

import Debug.Trace
import Text.ParserCombinators.ReadP

import System.IO
import System.Environment
import Control.Applicative
import System.Directory
import Data.Char  
import Data.List

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
      otherwise -> parseBKG <$> readFile fileName 
    
    case g of
        Left e -> print e
        Right r -> 
            case action of 
                PrintState -> printGrammer r
                FirstStep ->  print action
                SecondStep -> print action               
        
 
    --case action of
    --  PrintState -> printGrammer r
    --  FirstStep ->  print action
    --  SecondStep -> print action
    --return ()
    --where
    --parseInput = do 
    --  parseBKG <$> hGetContents stdin 


printGrammer Grammar{..} = do 
    printTerms nonterms
    printTerms terms
    putStrLn startNonterm
    printRules rules
    

printRules r = mapM_ (createRule) r  where
    createRule TRule{..} = putStrLn $ leftS ++ ("->" ++ rightS)

printTerms terms = putStrLn $ intercalate "," terms

parseArgs :: [String] -> Either String Config
parseArgs [] = Left $ "Not enough arguments."
parseArgs [i] 
    | i == "-i" = Right $ Config PrintState True ""
    | i == "-1" = Right $ Config FirstStep True ""
    | i == "-2" = Right $ Config SecondStep True ""
    | otherwise = Left $ "Unknown arguments"
parseArgs [i,n] 
    | i == "-i" = Right $ Config PrintState False n
    | i == "-1" = Right $ Config FirstStep False n
    | i == "-2" = Right $ Config SecondStep False n
    | otherwise = Left $ "Unknown arguments"
parseArgs _ = Left $ "Too many arguments."
