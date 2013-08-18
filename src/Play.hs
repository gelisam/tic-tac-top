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


class AI a => Playable a where
  markGame :: a -> String -> String
  printGame :: a -> String
  printChoices :: a -> String
  validateChoice :: a -> String -> Maybe (GameMove a)


printd :: String -> IO ()
printd s = do threadDelay 500000
              putStrLn s

inspect_game :: Playable a => a -> IO ()
inspect_game g = do putStrLn $ (if p then markGame g else id) $ printGame g
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
    p = current_player g

next_turn :: Playable a => a -> IO ()
next_turn g = do inspect_game g
                 if current_player g
                   then ai_to_play g
                   else user_to_play g

ai_to_play :: Playable a => a -> IO ()
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

user_to_play :: Playable a => a -> IO ()
user_to_play g = do when (winner == Just False && moves_left > 1) $ do
                      printd $ printf "You could win in %d moves.\n" ((moves_left + 1) `quot` 2)
                    when (winner == Just True && moves_left == 2) $ do
                      printd $ printf "Got you cornered!\n"
                    when (winner == Just True && moves_left == 1) $ do
                      printd $ printf "Checkmates!\n"
                    putStr $ printChoices g
                    putStr "> "
                    hFlush stdout
                    line <- getLine
                    case validateChoice g line of
                      Nothing -> do printd "That is not a legal move.\n"
                                    user_to_play g
                      Just m -> do putStrLn ""
                                   let g' = play g m
                                   let b' = printGame g'
                                   when (markGame g' b' /= b') $ do
                                     putStrLn $ b'
                                     threadDelay 500000
                                   next_turn g'
  where
    (_, winner, moves_left) = best_move g

play_game :: Playable a => a -> IO ()
play_game g = do printd "please wait while the computer evaluates your chances.\n"
                 next_turn g
