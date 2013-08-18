{-# OPTIONS -XTypeFamilies #-}
module Game where

import Player


type Winner = Maybe Player

class Game a where
  current_player :: a -> Player
  winner :: a -> Winner
  
  tie :: a -> Bool
  tie = null . legal_moves
  
  data GameIx a :: *
  game_range :: (GameIx a, GameIx a)
  game_index :: a -> GameIx a
  indexed_game :: GameIx a -> a
  
  data GameMove a :: *
  legal_moves :: a -> [GameMove a]
  play :: a -> GameMove a -> a
