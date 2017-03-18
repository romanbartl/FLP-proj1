module Parser
    where

import Types.BKG
import Text.ParserCombinators.ReadP

parseBKG :: String -> Either String Grammar
