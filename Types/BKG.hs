{-FLP 2017
  Functional project - SIMPLIFY-BKG
  Author : Studena Zuzana
  Login : xstude22
  Date : 31.3.2017
-}

module Types.BKG
    where

type TTerm = String
type TNonterm = String
type TermNontermComb = String

data TRule = TRule
    { leftS :: TNonterm
    , rightS :: TermNontermComb
    }
    deriving (Show)

data Grammar = Grammar
    { nonterms :: [TNonterm]
    , terms :: [TTerm]
    , startNonterm :: TNonterm
    , rules :: [TRule]
    } deriving (Show)

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

