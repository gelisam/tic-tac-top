module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Text.Printf
import System.Exit
import System.IO

import Player
import Board
import Game
import AI
import TicTacTop
import Play


main = do let b = indexed_board (fst board_range)
              g = TicTacTop 0 False b
          play_game g
