module Types.BKG
    where

type TTerm = String
type TNonTerm = String
type TermNontermComb = String

data TRule = TRule
    { leftS :: TTerm
    , rightS :: TermNontermComb
    }
    deriving (Show)

data Grammar = Grammar
    { nonTerms :: [TNonTerm]
    , terms :: [TTerm]
    , startNonTerm :: TNonTerm
    , rules :: [TRule]
    }
    deriving (Show)

data ProgAction 
    = PrintState
    | FirstStep
    | SecondStep
    deriving (Show)

data  Config = Config
    { pAction :: ProgAction
    , noInput :: Bool
    , inGrammar :: FilePath
    }   
    deriving (Show)