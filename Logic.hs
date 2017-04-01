{-# LANGUAGE RecordWildCards #-}
module Logic
    where

import Types.BKG
import Data.Char
import Data.List
import Data.Ord

-- First step of simplify grammar.
alg1 :: Grammar -> Grammar
alg1 g@Grammar{..} = Grammar newNonterms terms startNonterm newRules 
    where
        ntSet = getNtSet g []
        -- create new set of nonteminal and remove duplicate. 
        newNonterms = nub $ ((ntSet) ++ (startNonterm:[]))   
        -- creat new set of rules, if left side of rule is in Nt set and right side is combination of terminal and nonterminal symbols 
        newRules = [r | r <- rules, ((elem (leftS r) ntSet) && (isTermNonterm ntSet terms (rightS r))) ]   

-- Iterates until the final set of Nt formed. Nt is set ..
getNtSet :: Grammar -> [TNonterm] -> [TNonterm]
getNtSet g prevNt = if (length(prevNt) == length(newNt)) then prevNt else getNtSet g $ nub (prevNt ++ newNt) 
    where
        -- filter rules and create Ni set from left side of rules. 
        newNt = createNiSet g $ filterRules g prevNt

-- Create Ni set from left side of filter rules. 
createNiSet :: Grammar -> [TRule] -> [TNonterm]
createNiSet Grammar{..} vr = [nt | nt <- nonterms , any (\r -> nt == (leftS r)) vr ] 

filterRules :: Grammar -> [TNonterm] -> [TRule]
filterRules Grammar{..} prevNt = [r | r <- rules, ((isTermNonterm  prevNt terms (rightS r)) ) ]

-- Checks characters from right side of the rule. 
isTermNonterm :: [TNonterm] -> [TTerm] -> TermNontermComb -> Bool
isTermNonterm prevNt terms [] = True 
isTermNonterm prevNt terms (n:ns) = if (elem (n:[]) terms || (elem (n:[]) prevNt) || (n == '#')) then isTermNonterm prevNt terms ns else False 

------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Second part of simlify grammar. returns grammar without redundatn rules.
alg2 :: Grammar -> Grammar
alg2 g@Grammar{..} = Grammar newNT  newT startNonterm newR 
    where
        viSet = getViSet g (startNonterm:[])
        --creates new nonterms set from intersect of Vi nonterms 
        newNT = intersect viSet nonterms
        newT = intersect viSet terms
        newR = filterRulesVi rules viSet

-- Iterates until the final set of Vi formed. Vi is set ... TODO
getViSet :: Grammar -> [TermNontermComb] -> [TermNontermComb]  
getViSet g@Grammar{..} prevVi = if (length(prevVi) == length(newVi)) then prevVi else (getViSet g newVi) 
    where
        -- concatenate Vi-1 and Vi
        newVi = nub $ (prevVi ++ (createViSet g prevVi) )

-- Creates Vi set from terminal and nonterminal. 
-- Terminal or  Nonterminal is adds into set if intersect of the right side of the rule and Vi set is not empry.
createViSet :: Grammar -> [TermNontermComb] -> [TermNontermComb]
createViSet Grammar{..} prevVi = [vi | vi <- termNontermSet,  any (\r -> (not $ null $ intersect vi (rightS r))) filtRules] 
    where
        -- filter rules
        filtRules = filterRulesVi rules prevVi
        -- concatenate set of nonterms and terms
        termNontermSet = nonterms ++ terms
    
-- Filter rules with left side in Vi-1 set 
filterRulesVi :: [TRule] -> [TNonterm] -> [TRule]
filterRulesVi rules prevVi = [r | r <- rules, (elem (leftS r) prevVi) ]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Check syntax and semantics of parsed grammar
gCheck :: Grammar -> Either String Grammar
gCheck g@Grammar{..} = 
    if checkTerms && checkNonterms && checkStartNt && checkRulesLeft && checkRulesRight then Right g else Left "Incorrect grammar format." 
        where 
            -- Chcek if all Terminals are lower case or epsilon (#)
            checkTerms = (all (\t -> checkTerm [isLower,(=='#')]  t ) terms)  
            checkNonterms = (all (\t -> checkTerm [isUpper] t ) nonterms) 
            checkStartNt = elem startNonterm nonterms
            checkRulesLeft = (all (\r -> (elem (leftS r) nonterms)) rules)  
            checkRulesRight = (all (\r -> checkRSide (rightS r) terms nonterms ) rules) 

-- Check terminal or nonterminal syntax by a function list from first argument
checkTerm :: [(Char -> Bool)] -> String -> Bool
checkTerm [] (x:[]) = False
checkTerm (f:fs) (x:[]) = if f x then True else checkTerm fs (x:[])
checkTerm f (x:xs) = False
-- Check if right side of rule contains only terminal or nonterminal symbols
checkRSide :: TermNontermComb -> [TTerm] -> [TNonterm] -> Bool
checkRSide [] t n = True
checkRSide (r:rs) t n = if ((elem (r:[]) n) || (elem (r:[]) t) || (r == '#') )  then checkRSide rs t n else False 