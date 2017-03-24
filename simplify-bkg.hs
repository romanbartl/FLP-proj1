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
import Data.Ord

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
                PrintState -> printGrammar r
                FirstStep -> print $ prepareNTset r []
                SecondStep -> print action               
        
 
    --case action of
    --  PrintState -> printGrammer r
    --  FirstStep ->  print action
    --  SecondStep -> print action
    --return ()
    --where
    --parseInput = do 
    --  parseBKG <$> hGetContents stdin 

printGrammar Grammar{..} = do 
    printTerms nonterms
    printTerms terms
    putStrLn startNonterm
    printRules rules
    

printRules r = mapM_ (createRule) r  where
    createRule TRule{..} = putStrLn $ leftS ++ ("->" ++ rightS)

printTerms terms = putStrLn $ intercalate "," terms

--cpNtSets :: [TNonterm] -> [TNonterm] -> [TNonterm]
--cpNtSets [] [] = []
--cpNtSets [] l1 = l1 
--cpNtSets (l1:l1s) (l2:l2s) =  if l1==l2 then comapreLists l1s l2s else l1s 

--ntSets g nSet@(n0:ns) = 

--    nt <- [n | n <- nonterms , any (\r -> (elem (rightS r) terms) && ((leftS r) == n) ) rules]
--    if (n0 == nt || (null n0) ) then (return n0) else ntSets g (nt ++ nSet)



prepareNTset :: Grammar -> [TNonterm] -> [TNonterm]
prepareNTset g prevNt = if (length(prevNt) == length(newNt)) then newNt else prepareNTset g $ nub $ (prevNt ++ newNt) where
    newNt = createNtSet g $ validRules g prevNt

createNtSet :: Grammar -> [TRule] -> [TNonterm]
createNtSet Grammar{..} vr = [nt | nt <- nonterms , any (\r -> nt == (leftS r)) vr ] 

validRules :: Grammar -> [TNonterm] -> [TRule]
validRules Grammar{..} prevNt = [r | r <- rules, ((isTermNonterm  prevNt terms (rightS r)) ) ]

isTermNonterm :: [TNonterm] -> [TTerm] -> TermNontermComb -> Bool
isTermNonterm prevNt terms [] = True 
isTermNonterm prevNt terms (n:ns) = if (elem (n:[]) terms || (elem (n:[]) prevNt)) then isTermNonterm prevNt terms ns else False 



--endRule = (and (leftS == nt) (elem rightS terms)) 
comapreLists :: [[String]] -> [[String]] -> Bool
comapreLists [] [] = True
comapreLists [] l2 = False
comapreLists l1 [] = False
comapreLists (l1:l1s) (l2:l2s) = if l1==l2 then comapreLists l1s l2s else False 

prepareNTset1 :: Grammar -> [TNonterm]
prepareNTset1 Grammar{..} = [nt | nt <- nonterms , any (\r -> (elem (rightS r) terms) && ((leftS r) == nt) ) rules]


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
