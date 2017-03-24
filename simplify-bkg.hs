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
                FirstStep -> do
                    printGrammar $ alg1 r 
                    --print $ getNtSet r []
                SecondStep -> do
                  --print $ func r ["S"]
                  --print $ createViSet r ["S"] 
                  --print $ getViSet r ["S"]
                  printGrammar $ alg2 $ alg1 r             
    --
 
    --case action of
    --  PrintState -> printGrammer r
    --  FirstStep ->  print action
    --  SecondStep -> print action
    --return ()
    --where
    --parseInput = do 
    --  parseBKG <$> hGetContents stdin 

-----------------------------------------------------------------------------------------
printGrammar Grammar{..} = do 
    printTerms nonterms
    printTerms terms
    putStrLn startNonterm
    printRules rules
    

printRules r = mapM_ (createRule) r  where
    createRule TRule{..} = putStrLn $ leftS ++ ("->" ++ rightS)

printTerms terms = putStrLn $ intercalate "," terms
---------------------------------------------------------------------------------------

alg1 :: Grammar -> Grammar
alg1 g@Grammar{..} = Grammar newNonterms terms startNonterm newRules where
    ntSet = getNtSet g []
    newNonterms = nub $ ((ntSet) ++ (startNonterm:[]))   
    newRules = [r | r <- rules, ((elem (leftS r) ntSet) && (isTermNonterm ntSet terms (rightS r))) ]   

getNtSet :: Grammar -> [TNonterm] -> [TNonterm]
getNtSet g prevNt = if (length(prevNt) == length(newNt)) then prevNt else getNtSet g $ nub (prevNt ++ newNt) where
    newNt = createNtSet g $ valitRulesNt g prevNt

createNtSet :: Grammar -> [TRule] -> [TNonterm]
createNtSet Grammar{..} vr = [nt | nt <- nonterms , any (\r -> nt == (leftS r)) vr ] 

valitRulesNt :: Grammar -> [TNonterm] -> [TRule]
valitRulesNt Grammar{..} prevNt = [r | r <- rules, ((isTermNonterm  prevNt terms (rightS r)) ) ]

isTermNonterm :: [TNonterm] -> [TTerm] -> TermNontermComb -> Bool
isTermNonterm prevNt terms [] = True 
isTermNonterm prevNt terms (n:ns) = if (elem (n:[]) terms || (elem (n:[]) prevNt) || (n == '#')) then isTermNonterm prevNt terms ns else False 

-------------------------------------------------------------------------------------

alg2 :: Grammar -> Grammar
alg2 g@Grammar{..} = Grammar newNT  newT startNonterm newR where
    newNT = intersect (getViSet g (startNonterm:[])) nonterms
    newT = intersect (getViSet g (startNonterm:[])) terms
    newR = validRulesVi rules (getViSet g (startNonterm:[]))
    
getViSet :: Grammar -> [TermNontermComb] -> [TermNontermComb]  
getViSet g@Grammar{..} prevVi = if (length(prevVi) == length(newVi)) then prevVi else (getViSet g newVi) where
    newVi = nub $ (prevVi ++ (createViSet g prevVi) )

createViSet :: Grammar -> [TermNontermComb] -> [TermNontermComb]
createViSet Grammar{..} prevVi = [vi | vi <- termNontermSet,  any (\r -> (not $ null $ intersect vi (rightS r)) ) (validRulesVi rules prevVi) ]  where
    termNontermSet = nonterms ++ terms
    
--vycleny pravidla u ktorych je na lavej strane iba Nererminal z mnoziny Vi-1
validRulesVi :: [TRule] -> [TNonterm] -> [TRule]
validRulesVi rules prevVi = [r | r <- rules, (elem (leftS r) prevVi) ]



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


--Unused code
--endRule = (and (leftS == nt) (elem rightS terms)) 
comapreLists :: [[String]] -> [[String]] -> Bool
comapreLists [] [] = True
comapreLists [] l2 = False
comapreLists l1 [] = False
comapreLists (l1:l1s) (l2:l2s) = if l1==l2 then comapreLists l1s l2s else False 

getNtSet1 :: Grammar -> [TNonterm]
getNtSet1 Grammar{..} = [nt | nt <- nonterms , any (\r -> (elem (rightS r) terms) && ((leftS r) == nt) ) rules]
