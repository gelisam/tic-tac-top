-- exploring the entire state space,
-- because the board is small enough for that.
{-# OPTIONS -XScopedTypeVariables -XDefaultSignatures -XFlexibleContexts #-}
module AI where

import Data.Array
import Data.Function
import Data.List

import Game


type BestMove a = (Maybe (GameMove a), Winner, Int)

-- the best move at each game state, for each player.
-- True wants to delay the conclusion of the game, while
-- False wants to precipitate it.
-- Both want to win.
mkBestMoves :: forall a. (Game a, Ix (GameIx a))
            => Array (GameIx a) (BestMove a)
            -> Array (GameIx a) (BestMove a)
mkBestMoves memo = array game_range [(ix, f ix) | ix <- range game_range]
  where
    f = best_move . indexed_game
    
    best_move :: (Game a, Ix (GameIx a))
              => a
              -> BestMove a
    best_move g = case winner g of
                    Nothing -> best_from g
                    Just p  -> (Nothing, Just p, 0)
      where
        best_from = maximumBy (compare `on` value) . (tie:) . map outcome . legal_moves
        tie = (Nothing, Nothing, 0)
        outcome m = let (     _, winner, moves_left  ) = response m
                     in (Just m, winner, moves_left+1)
        value t = win t + delay t
        win (Nothing, Nothing, _)                    = -3000
        win (_, winner, _) | winner == Just opponent = -2000
        win (_, winner, _) | winner == Nothing       = -1000
        win (_, winner, _) | otherwise               = 0
        delay (_, _, moves_left) = if cur_player then moves_left else -moves_left
        -- lookup the response in the _memoized_ version
        -- (a recursive call to mkBestMoves would be slow)
        response = (memo !) . game_index . play g
        opponent = not cur_player
        cur_player = current_player g


class Game a => AI a where
  best_moves :: Array (GameIx a) (BestMove a)
  best_move :: a -> BestMove a
  
  default best_moves :: Ix (GameIx a) => Array (GameIx a) (BestMove a)
  best_moves = fix mkBestMoves
  
  default best_move :: Ix (GameIx a) => a -> BestMove a
  best_move = (best_moves!) . game_index
