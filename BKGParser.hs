{-FLP 2017
  Functional project - SIMPLIFY-BKG
  Author : Studena Zuzana
  Login : xstude22
  Date : 31.3.2017
-}

{-# LANGUAGE RecordWildCards #-}
module BKGParser
    where

import Types.BKG
import Text.ParserCombinators.ReadP

parseBKG :: String -> Either String Grammar
parseBKG inG = case readP_to_S bkgParser inG of 
    [(bkg, _)] -> Right bkg
    _          -> Left "Parsing error."

-- parse input grammar
bkgParser :: ReadP Grammar 
bkgParser = do 
    nonterms <- parseTerms
    newLine
    terms <- parseTerms
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