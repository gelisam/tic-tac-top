module Play where

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


printd :: String -> IO ()
printd s = do threadDelay 500000
              putStrLn s

markBoard :: TicTacTop -> String -> String
markBoard g = unlines . mark . lines where
  mark xs = zipWith (++) xs marks
  marks = replicate (minRow g) " (forbidden)" ++ repeat ""

inspect_game :: TicTacTop -> IO ()
inspect_game g = do putStrLn $ (if player g then markBoard g else id) $ printBoard b
                    case winner g of
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

next_turn :: TicTacTop -> IO ()
next_turn g = do inspect_game g
                 if player g
                   then ai_to_play g
                   else user_to_play g

ai_to_play :: TicTacTop -> IO ()
ai_to_play g = do when (winner == Just True && moves_left > 1) $ do
                    printd $ printf "I think I can win in %d moves.\n" (moves_left `quot` 2)
                  when (winner == Just False && moves_left == 2) $ do
                    printd $ printf "Clever trap!\n"
                  when (winner == Just False && moves_left == 1) $ do
                    printd $ printf "You tricked me!\n"
                  let g' = play g m
                  threadDelay 500000
                  next_turn g'
  where
    (Just m, winner, moves_left) = best_move g

user_to_play :: TicTacTop -> IO ()
user_to_play g = do when (winner == Just False && moves_left > 1) $ do
                      printd $ printf "You could win in %d moves.\n" ((moves_left + 1) `quot` 2)
                    when (winner == Just True && moves_left == 2) $ do
                      printd $ printf "Got you cornered!\n"
                    when (winner == Just True && moves_left == 1) $ do
                      printd $ printf "Checkmates!\n"
                    putStr $ markBoard g $ unlines choice_grid
                    putStr "> "
                    hFlush stdout
                    line <- getLine
                    let choice = if null line then '.' else head line
                    case lookup choice choices of
                      Nothing -> do printd "That is not a legal move.\n"
                                    user_to_play g
                      Just m -> do putStrLn ""
                                   let g' = play g m
                                   when (minRow g' > 0) $ do
                                     putStrLn $ printBoard $ board g'
                                     threadDelay 500000
                                   next_turn g'
  where
    b = board g
    (_, winner, moves_left) = best_move g
    
    full_choice_grid = ["123","456","789"]
    choice_grid = (map.map) dot_if_invalid full_choice_grid
    dot_if_invalid c = if valid_choice c then c
                                         else cell_at c
    valid_choice k = k `elem` map fst choices
    choices = [(full_choice_grid `at` movePos m, m) | m <- legal_moves g]
    cell_at = printCell . fromJust . flip lookup cell_map
    cell_map = [(full_choice_grid `at` m, b `at` m) | m <- positions]

play_game :: TicTacTop -> IO ()
play_game g = do printd "please wait while the computer evaluates your chances.\n"
                 next_turn g
