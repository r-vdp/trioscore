{-# OPTIONS_GHC -O2 -Wall #-}

import System.IO
import Control.Monad.Writer
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


handleInput :: [String] -> Scores -> Writer String Scores
handleInput []        s = return s
handleInput ["del",x] s = return (deleteScore x s)
handleInput [x,y]     s = do score <- parseScore y
                             return (addScore x score s)
handleInput _         s = unknown s

unknown :: Scores -> Writer String Scores
unknown s = tell "Unknown command!" >> return s

parseScore :: String -> Writer String Integer
parseScore string = case reads string of
                      [(score, _)] -> return score
                      _            -> tell "Could not parse score!" >> return 0

run :: Scores -> IO ()
run scores = do
  putStr "Enter score: "
  hFlush stdout
  ws <- fmap words getLine
  parseLine ws scores

parseLine :: [String] -> Scores -> IO ()
parseLine line
  | isEnd line = (const . return) ()
  | otherwise  = rerun . runWriter . (handleInput line)

rerun :: (Scores, String) -> IO ()
rerun (scores, msg) = printError msg >> print scores >> run scores

printError :: String -> IO ()
printError "" = return ()
printError s  = putStrLn s

isEnd :: [String] -> Bool
isEnd = (["end"] ==) . take 1

main :: IO ()
main = run emptyScores
 
