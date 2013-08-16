module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Array
import Data.Function
import Data.Ix
import Data.List
import Data.Maybe
import Text.Printf
import System.Exit
import System.IO

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

tie :: GameState -> Bool
tie = null . legal_moves


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


printd :: String -> IO ()
printd s = do threadDelay 500000
              putStrLn s

inspect_game :: GameState -> IO ()
inspect_game g = do putStrLn $ printBoard b
                    case winner b of
                      Nothing    -> return ()
                      Just True  -> do printd "The computer wins."
                                       exitSuccess
                      Just False -> do printd "You win!"
                                       exitSuccess
                    when (tie g) $ do
                      printd "It's a tie."
                      exitSuccess
  where
    b = board g

next_turn :: GameState -> IO ()
next_turn g = do inspect_game g
                 if player g
                   then ai_to_play g
                   else user_to_play g

ai_to_play :: GameState -> IO ()
ai_to_play g = do when (winner == Just True && moves_left > 1) $ do
                    printd $ printf "I think I can win in %d moves.\n" (moves_left `quot` 2)
                  when (winner == Just False && moves_left == 2) $ do
                    printd $ printf "Clever trap!\n"
                  when (winner == Just False && moves_left == 1) $ do
                    printd $ printf "You tricked me!\n"
                  threadDelay 500000
                  next_turn $ play g m
  where
    (m, winner, moves_left) = best_move g

user_to_play :: GameState -> IO ()
user_to_play g = do when (winner == Just False && moves_left > 1) $ do
                      printd $ printf "You could win in %d moves.\n" ((moves_left + 1) `quot` 2)
                    when (winner == Just True && moves_left == 2) $ do
                      printd $ printf "Got you cornered!\n"
                    when (winner == Just True && moves_left == 1) $ do
                      printd $ printf "Checkmates!\n"
                    printd $ intercalate "\n" choice_grid
                    putStr "> "
                    hFlush stdout
                    choice:_ <- getLine
                    case lookup choice choices of
                      Nothing -> do printd "That is not a legal move.\n"
                                    user_to_play g
                      Just m -> do putStrLn ""
                                   next_turn $ play g m
  where
    b = board g
    (_, winner, moves_left) = best_move g
    
    full_choice_grid = ["123","456","789"]
    choice_grid = (map.map) dot_if_invalid full_choice_grid
    dot_if_invalid c = if valid_choice c then c
                                         else cell_at c
    valid_choice k = k `elem` map fst choices
    choices = [(full_choice_grid `at` m, m) | m <- legal_moves g]
    cell_at = printCell . fromJust . flip lookup cell_map
    cell_map = [(full_choice_grid `at` m, b `at` m) | m <- positions]

main = do let b = indexed_board (fst board_range)
              g = GameState 0 False b
          printd "please wait while the computer evaluates your chances.\n"
          next_turn g
