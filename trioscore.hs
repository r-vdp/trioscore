{-# OPTIONS_GHC -O2 -Wall -Werror #-}

import System.IO
import qualified Data.Map as Map

type ScoreMap = Map.Map String Integer

newtype Scores = Scores ScoreMap
    deriving (Eq, Ord)

instance Show Scores where
    show scores = "\nCurrent scores: " ++ showScores scores

showScores :: Scores -> String
showScores (Scores scoreMap)
    | Map.null scoreMap = "none\n"
    | otherwise         = unlines . (map formatPair) . Map.toList $ scoreMap

formatPair :: (Show a) => (String, a) -> String
formatPair (s, a) = '\n' : '\t' : (s ++ ": " ++ show a)

toMap :: Scores -> ScoreMap
toMap (Scores scoreMap) = scoreMap

addScore :: String -> Integer -> Scores -> Scores
addScore name score = Scores . (Map.insertWith (+) name score) . toMap

deleteScore :: String -> Scores -> Scores
deleteScore name = Scores . (Map.delete name) . toMap


handleInput :: [String] -> Scores -> Scores
handleInput []          = id
handleInput [_]         = id
handleInput ("del":x:_) = deleteScore x
handleInput (x:y:_)     = addScore x (parseScore y)

parseScore :: String -> Integer
parseScore string = case reads string of
                        [(score, _)] -> score
                        _            -> 0

run :: Scores -> IO ()
run scores = do
    putStr "Enter score: "
    hFlush stdout
    line <- getLine
    parseLine (words line) scores

parseLine :: [String] -> Scores -> IO ()
parseLine line scores
    | isEnd line = return ()
    | otherwise  = let newScores = handleInput line scores
                   in print newScores >> run newScores

isEnd :: [String] -> Bool
isEnd ("end":_) = True
isEnd _         = False

main :: IO ()
main = (run . Scores) Map.empty
 
