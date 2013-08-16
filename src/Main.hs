module Main where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Function
import Data.Ix
import Data.List

import Board


type Winner = Maybe Player

winner :: Board -> Winner
winner b = h_loser
       <|> v_winner
       <|> d1_winner
       <|> d2_winner
  where
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



data GameState = GameState
  { minRow :: Int
  , player :: Player
  , board :: Board
  }
type GameStateIx = (Int, PlayerIx, BoardIx)

game_range :: (GameStateIx, GameStateIx)
game_range = (lo, hi)
  where
    lo = (0, fst player_range, fst board_range)
    hi = (2, snd player_range, snd board_range)

game_index :: GameState -> GameStateIx
game_index (GameState m p b) = (m, player_index p, board_index b)

indexed_game :: GameStateIx -> GameState
indexed_game (m, p, b) = GameState m (indexed_player p) (indexed_board b)


type Move = Pos

legal_moves :: GameState -> [Move]
legal_moves g = filter legal positions
  where
    legal p = unoccupied p
           && above_min p
    unoccupied p = (board g `at` p) == Nothing
    above_min p = snd p >= minRow g

play :: GameState -> Move -> GameState
play (GameState minRow  player  board) (x, y) =
      GameState minRow' player' board'
  where
    y' = y + 1
    minRow' = if y' `elem` pos_range then y' else 0
    player' = not player
    board'  = set_at (x, y) (Just player) board


-- the best move at each game state, for each player.
-- True wants to delay the conclusion of the game, while
-- False wants to precipitate it.
-- Both want to win.
best_moves :: Array GameStateIx (Move, Winner, Int)
best_moves = array game_range $ map f $ range game_range
  where
    f ix = (ix, best_move $ indexed_game ix)

best_move :: GameState -> (Move, Winner, Int)
best_move g = case winner $ board g of
                Nothing -> best_from g
                Just p  -> ((-1, -1), Just p, 0)
  where
    best_from = maximumBy (compare `on` value) . (tie:) . map outcome . legal_moves
    tie = ((-2,-2), Nothing, 0)
    outcome m = let (_, winner, moves_left) = response m
                 in (m, winner, moves_left+1)
    value ((-2,-2), _, _)                          = -3000
    value (_, winner, _) | winner == Just opponent = -2000
    value (_, winner, _) | winner == Nothing       = -1000
    value (_, _, moves_left) = if cur_player then moves_left else -moves_left
    response = (best_moves !) . game_index . play g
    opponent = not cur_player
    cur_player = player g


main = do b <- readBoard <$> getContents
          let g = GameState 0 True b
              (m, winner, _) = best_move g
          case winner of
            Just p -> putStrLn $ printPlayer p : " wins!"
            Nothing -> let g' = play g m
                           b' = board g'
                        in putStr $ printBoard b'
