module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Text.Printf
import System.Exit
import System.Environment
import System.IO

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

enumerate_responses :: TicTacTop -> [(String, Char)]
enumerate_responses g = immediate_responses ++ subsequent_responses
  where
    immediate_responses = [([m], immediate_response m) | m <- player_moves]
    subsequent_responses = [(p ++ p', r') | (p, r) <- immediate_responses
                                          , r `elem` "123456789"
                                          , let g' = playLetter r
                                          , (p', r') <- enumerate_responses g'
                                          ]
    playLetter c = play g $ letterMove c
    player_moves = filter (`elem` "123456789") $ printChoices g
    immediate_response = letter . best_move . play g . letterMove
    letter (Just (GameMove pos), _, _) = letterGrid `at` pos
    letter (Nothing, _, _) = printCell $ winner g


main = do let b = indexed_board (fst board_range)
          mapM_ print $ enumerate_responses $ TicTacTop 0 False b
