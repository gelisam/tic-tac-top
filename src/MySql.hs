module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Text.Printf
import System.Exit
import System.Environment
import System.IO
import Debug.Trace

import Player
import Board
import Game
import AI
import TicTacTop
import Play


-- INSERT INTO  `carlosm5_board`.`AI` (
-- `game` ,
-- `input` ,
-- `response`
-- )
-- VALUES (
-- 'tic-tac-top',  '17',  '4'
-- );

enumerate_responses :: String -> TicTacTop -> [(String, Char)]
enumerate_responses prefix g = concatMap pathsFrom $ legal_moves g
  where
    pathsFrom :: GameMove TicTacTop -> [(String, Char)]
    pathsFrom m | isJust response = (prefix', letter') : enumerate_responses prefix' g''
      where
        g' = play g m
        g'' = play g' m'
        (response, _, _) = best_move g'
        Just m' = response
        letter = letterGrid `at` movePos m
        letter' = letterGrid `at` movePos m'
        prefix' = prefix ++ [letter]
    pathsFrom m | otherwise = []


main = do let b = indexed_board (fst board_range)
          mapM_ print $ enumerate_responses "" $ TicTacTop 0 False b
