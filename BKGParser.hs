{-# LANGUAGE RecordWildCards #-}
module BKGParser
    where

import Debug.Trace
import Types.BKG
import Data.Char
import Text.ParserCombinators.ReadP

debug = flip trace

parseBKG :: String -> Either String Grammar
parseBKG inG = case readP_to_S bkgParser inG of 
    [(bkg, _)] -> Right bkg
    _          -> Left "Parsing error."

bkgParser :: ReadP Grammar 
bkgParser = do 
    nonterms <- parseTerms
    --traceM "neterminaly:"
    --traceShowM nonterms 
    newLine
    terms <- parseTerms
    --traceM "terminaly:"
    --traceShowM terms 
    newLine
    startNonterm <- parseTerm
    newLine
    rules <- parseRules
    eof
    return $ Grammar nonterms terms startNonterm rules

newLine =  char '\n'
comma = char ','
arrow = string "->"

parseTerm = many1 $ satisfy satisfyEndLine
parseTerms = sepBy1 parseTerm comma 
parseRules = many1 $ do
    r <- parseRule
    newLine
    return r
    where 
        parseRule = do
            leftSide <- many1 $ satisfy (/= '-')
            arrow
            rightSide <- parseTerm
            return $ TRule leftSide rightSide

-- Set end line character
satisfyEndLine :: Char -> Bool
satisfyEndLine c = ((c /= '\n') && (c /= ','))

-- Check syntax and semntic of parsed grammar
gCheck :: Grammar -> Either String Grammar
gCheck g@Grammar{..} = if checkTerms && checkNonterms && checkStartNt && checkRulesLeft && checkRulesRight then Right g else Left "Incorrect grammar format." 
                        where 
                            checkTerms = (all (\t -> checkTerm [isLower,(=='#')]  t ) terms)  
                            checkNonterms = (all (\t -> checkTerm [isUpper] t ) nonterms) 
                            checkStartNt = elem startNonterm nonterms
                            checkRulesLeft = (all (\r -> (elem (leftS r) nonterms)) rules)  
                            checkRulesRight = (all (\r -> checkRSide (rightS r) terms nonterms ) rules) 

-- Check term and nonterm syntax 
checkTerm :: [(Char -> Bool)] -> String -> Bool
checkTerm [] (x:[]) = False
checkTerm (f:fs) (x:[]) = if f x then True else checkTerm fs (x:[])
checkTerm f (x:xs) = False

-- Check if right side of rule contains only terminal or nonterminal symbols
checkRSide :: TermNontermComb -> [TTerm] -> [TNonterm] -> Bool
checkRSide [] t n = True
checkRSide (r:rs) t n = if ((elem (r:[]) n) || (elem (r:[]) t) || (r == '#') )  then checkRSide rs t n else False 