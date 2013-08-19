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
import TicTacTop2
import Play


main = do let b = indexed_board (fst board_range)
          args <- getArgs
          case args of
            ["tic-tac-top"]  -> play_game $ TicTacTop        0 False b
            ["tic-tac-top2"] -> play_game $ TicTacTop2 False 0 False b
            _                  -> do
              name <- getProgName
              printf "usage: %s <game>\n" name
              printf "\n"
              printf "Play a short console-based board game against the computer.\n"
              printf "Available games include:\n"
              printf "\n"
              printf "  tic-tac-top\n"
              printf "  tic-tac-top2\n"
