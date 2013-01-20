{-# OPTIONS_GHC -O2 -Wall #-}

import Control.Applicative
import Control.Monad.Writer
import System.IO

import Scores


handleInput :: [String] -> Scores -> Writer String Scores
handleInput []        = pure
handleInput ["del",x] = pure . deleteScore x
handleInput [x,y]     = liftA3 addScore (pure x) (parseScore y) . pure
handleInput _         = unknown

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
  | otherwise  = rerun . runWriter . handleInput line

rerun :: (Scores, String) -> IO ()
rerun (scores, msg) = printError msg >> print scores >> run scores

printError :: String -> IO ()
printError "" = return ()
printError s  = putStrLn s

isEnd :: [String] -> Bool
isEnd = (["end"] ==) . take 1

main :: IO ()
main = run emptyScores
 
