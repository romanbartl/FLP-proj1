module BKGParser
    where

import Debug.Trace
import Types.BKG
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

satisfyEndLine :: Char -> Bool
satisfyEndLine c = ((c /= '\n') && (c /= ',')) 
