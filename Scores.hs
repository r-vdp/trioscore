{-# OPTIONS_GHC -O2 -Wall #-}

module Scores
  ( Scores
  , addScore
  , deleteScore
  , emptyScores
  ) where

import qualified Data.Map as Map

type ScoreMap = Map.Map String Integer

newtype Scores = Scores { getScores :: ScoreMap } deriving (Eq, Ord)

instance Show Scores where
  show scores = "Current scores:\n" ++ showScores scores

showScores :: Scores -> String
showScores (Scores scoreMap)
  | Map.null scoreMap = "none\n"
  | otherwise         = unlines . (map formatPair) . Map.toList $ scoreMap

formatPair :: (Show a) => (String, a) -> String
formatPair (s, a) = '\t' : (s ++ ": " ++ show a)

addScore :: String -> Integer -> Scores -> Scores
addScore name score = Scores . (Map.insertWith (+) name score) . getScores

deleteScore :: String -> Scores -> Scores
deleteScore name = Scores . (Map.delete name) . getScores

emptyScores :: Scores
emptyScores = Scores Map.empty


