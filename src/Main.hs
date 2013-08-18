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


instance Playable TicTacTop where
  markGame g = unlines . mark . lines where
    mark xs = zipWith (++) xs marks
    marks = replicate (minRow g) " (forbidden)" ++ repeat ""
  
  printGame = printBoard . board
  
  printChoices g = print_marked letterBoard
    where
      print_marked = markGame g . unlines
      letterBoard = (map.map) dot_if_illegal letterGrid
      dot_if_illegal c = if letterMove c `elem` legal_moves g
                           then c
                           else '.'
  
  validateChoice g [c] | c `elem` "123456789" = if letterMove c `elem` legal_moves g
                                                  then Just (letterMove c)
                                                  else Nothing
  validateChoice g _ = Nothing

letterGrid :: [[Char]]
letterGrid = ["123","456","789"]

letterMove :: Char -> GameMove TicTacTop
letterMove = GameMove . head . matching_positions
  where
    matching_positions c = filter (\p -> letterGrid `at` p == c) positions


main = do let b = indexed_board (fst board_range)
              g = TicTacTop 0 False b
          play_game g
