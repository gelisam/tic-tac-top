{-# OPTIONS -XTypeFamilies #-}
module TicTacTop3 where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Function
import Data.List

import Player
import Board
import Game
import AI
import Play


data TicTacTop3 = TicTacTop3
  { minPos :: Pos
  , player :: Player
  , board :: Board
  }

instance Game TicTacTop3 where
  current_player = player
  
  winner g = h_loser
         <|> v_winner
         <|> d1_winner
         <|> d2_winner
    where
      b = board g
      h_loser = fmap not h_winner
      h_winner  = lookup_winners [[(x,  y) | x <- pos_range] | y <- pos_range]
      v_winner  = lookup_winners [[(x,  y) | y <- pos_range] | x <- pos_range]
      d1_winner = lookup_winners [[(i,  i) | i <- pos_range]]
      d2_winner = lookup_winners [[(i,2-i) | i <- pos_range]]
      
      lookup_winners :: [[Pos]] -> Maybe Player
      lookup_winners = msum . map lookup_winner
      
      lookup_winner :: [Pos] -> Maybe Player
      lookup_winner = list_winner . map (b `at`)
      
      list_winner xs = case nub xs of
        [maybe_p] -> maybe_p
        _         -> Nothing
  
  
  data GameIx TicTacTop3 = GameIx
     { minPosIx :: Pos
     , playerIx :: PlayerIx
     , boardIx  :: BoardIx
     } deriving (Eq, Ord, Ix)
  
  game_range = (lo, hi)
    where
      lo = GameIx (0,0) (fst player_range) (fst board_range)
      hi = GameIx (2,2) (snd player_range) (snd board_range)
  
  game_index (TicTacTop3 m p b) = GameIx
                                { minPosIx = m
                                , playerIx = player_index p
                                , boardIx  = board_index b
                                }
  
  indexed_game (GameIx m p b) = TicTacTop3
                              { minPos = m
                              , player = indexed_player p
                              , board  = indexed_board b
                              }
  
  
  newtype GameMove TicTacTop3 = GameMove
      { movePos :: Pos
      } deriving (Eq, Ord)
  
  legal_moves g = map GameMove $ filter legal positions
    where
      legal p = unoccupied p
             && above_min p
      unoccupied p = (board g `at` p) == Nothing
      above_min p = fst p >= fst (minPos g)
                 && snd p >= snd (minPos g)
  
  play g m = g''
    where
      (x, y) = movePos m
      (x', y')  = (x, y)
      (x'', y'') = if null (legal_moves g') then (0,0) else (x',y')
      TicTacTop3 minPos player board = g
      g'  = TicTacTop3 (x', y') player' board'
      g'' = TicTacTop3 (x'', y'') player' board'
      player' = not player
      board'  = set_at (x, y) (Just player) board


instance AI TicTacTop3

instance Playable TicTacTop3 where
  markGame g = unlines . mark2 . mark . lines where
    mark xs = zipWith (++) xs marks
    marks = replicate (snd $ minPos g) " x" ++ repeat ""
    mark2 = if fst (minPos g) > 0
              then (++[replicate (fst $ minPos g) 'x'])
              else id
  
  printGame = printBoard . board
  
  printChoices g = print_marked choiceBoard
    where
      print_marked = markGame g . unlines
      choiceBoard = (map.map) dot_if_illegal letterGrid
      dot_if_illegal c = if letterMove c `elem` legal_moves g
                           then c
                           else letterBoard `at` letterPos c
      letterBoard = (map.map) printCell (board g)
  
  validateChoice g [c] | c `elem` "123456789" = if letterMove c `elem` legal_moves g
                                                  then Just (letterMove c)
                                                  else Nothing
  validateChoice g _ = Nothing

letterGrid :: [[Char]]
letterGrid = ["123","456","789"]

letterPos :: Char -> Pos
letterPos = head . matching_positions
  where
    matching_positions c = filter (\p -> letterGrid `at` p == c) positions

letterMove :: Char -> GameMove TicTacTop3
letterMove = GameMove . letterPos
