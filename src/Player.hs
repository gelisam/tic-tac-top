-- we only support two-player games.
module Player where


type Player = Bool
type PlayerIx = Int

player_range :: (PlayerIx, PlayerIx)
player_range = (1,2)

player_index :: Player -> PlayerIx
player_index p = if p then 1 else 2

indexed_player :: PlayerIx -> Player
indexed_player 1 = True
indexed_player 2 = False
