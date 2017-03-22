module Types.BKG
    where

type TTerm = String
type TNonterm = String
type TermNontermComb = String

data TRule = TRule
    { leftS :: TTerm
    , rightS :: TermNontermComb
    }
    deriving (Show)

data Grammar = Grammar
    { nonterms :: [TNonterm]
    , terms :: [TTerm]
    , startNonterm :: TNonterm
    , rules :: [TRule]
    } deriving (Show)

-- instance Show Grammar 
--    where show nonterms = showsPrec nonterms 

data ProgAction 
    = PrintState
    | FirstStep
    | SecondStep
    deriving (Show)

data  Config = Config
    { action :: ProgAction
    , noInput :: Bool
    , fileName :: FilePath
    }   
    deriving (Show)

