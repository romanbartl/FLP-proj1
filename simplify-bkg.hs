{-# LANGUAGE RecordWildCards #-}

module Main
    (main)
where

import Debug.Trace

import System.IO
import System.Environment
import Control.Applicative
import System.Directory
import Data.List

import Types.BKG
import BKGParser
import Logic

main :: IO ()
main = do  
   conf <- parseArgs <$> getArgs 
   -- check program arguments
   case conf of 
      Left err -> print err
      Right c -> handleBKG c
           
-- Main program logic. First reads, parses and checks input grammar.
-- If no problem occures, calls right program function and prints output.
handleBKG :: Config -> IO ()
handleBKG Config{..} = do 
    -- get content of input file or stdin 
    g <- case noInput of  
      True -> parseBKG <$> hGetContents stdin 
      otherwise -> parseBKG <$> readFile fileName 
    
    -- check parsing errors
    case g of
        Left err -> print err
        Right r -> 
            -- check input grammar syntax and semantics 
            case gCheck r of 
                Left err -> print err 
                Right finalGrammar -> printGrammar $ programStep finalGrammar action  

programStep :: Grammar -> ProgAction -> Grammar
programStep g action = 
    case action of 
        PrintState -> g
        FirstStep -> alg1 g 
        SecondStep -> alg2 $ alg1 g             

-----------------------------------------------------------------------------------------

printGrammar :: Grammar -> IO ()
printGrammar Grammar{..} = do 
    printTerms nonterms
    printTerms terms
    putStrLn startNonterm
    printRules rules

printRules r = mapM_ (createRule) r  where
    createRule TRule{..} = putStrLn $ leftS ++ ("->" ++ rightS)

printTerms terms = putStrLn $ intercalate "," terms

-- parse program arguments
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