implementation module ligrettoModel

from   StdMisc import abort
import Math.Random		// for generating random numbers
import StdEnv
import Data.Maybe, GenEq
import iTasks._Framework.Generic.Visualization

derive gEq Card, SideUp, Color
derive gText Color

play_concealed_pile :: !Color !GameSt -> GameSt
play_concealed_pile color gameSt
  = set_player player` gameSt
where
	player	= get_player color gameSt
	player` = case player.hand.conceal of
				[] = shuffle_hand  player
				_  = swap_discards player

play_hand_card :: !Color !GameSt -> GameSt
play_hand_card color gameSt=:{GameSt | middle}
= case top_discard player of
      Nothing
        = gameSt
      (Just card)
        = case matching_piles card middle of
            []                 = gameSt
            [(pileno, pile):_] = let player` = remove_top_of_discard player
                                     middle` = updateAt pileno [card:pile] middle
                                  in set_player player` {GameSt | gameSt & middle = middle`}
where
	player = get_player color gameSt

play_row_card :: !Color !Int !GameSt -> GameSt
play_row_card color cardno gameSt=:{GameSt | middle}
  = case matching_piles card middle of
      []                 = gameSt
      [(pileno, pile):_] = let player` = move_ligretto_card_to_row cardno player
                               middle` = updateAt pileno [card:pile] middle
                            in set_player player` {GameSt | gameSt & middle  = middle`}
where
	player	= get_player color gameSt
	card    = row_card cardno player

get_player :: !Color !GameSt -> Player
get_player color gameSt=:{GameSt | players}
	= case [player \\ player <- players | player.color === color] of
	     [player : _] = player
	     ouch         = abort ("Could not find player with color " <+++ color)

set_player :: !Player !GameSt -> GameSt
set_player player gameSt=:{GameSt | players}
	= {GameSt | gameSt & players = [if (p.Player.color === player.Player.color) player p \\ p <- players]}

no_of_cards_in_row :: !NoOfPlayers -> Int
no_of_cards_in_row 2 = 5
no_of_cards_in_row 3 = 4
no_of_cards_in_row 4 = 3
no_of_cards_in_row n = abort ("ligretto.no_of_cards_in_row: illegal integer argument (" +++ toString n +++ ").\n")

all_colors :: [Color]
all_colors = [Red,Green,Blue,Yellow]

colors :: !NoOfPlayers -> [Color]
colors no_of_players = take no_of_players all_colors

initial_player_cards :: !NoOfPlayers !Color -> Pile
initial_player_cards no_of_players back
	= [{back=back,front=color,no=no} \\ color <- all_colors, no <- [1..10]]

shuffle :: ![a] !Int -> [a]
shuffle xs seed
	= fst (unzip (sortBy (\(_,r1) (_,r2) -> (r1 < r2)) (zip2 xs (genRandInt (abs seed)))))

initial_player :: !NoOfPlayers !Color !String !Int -> Player
initial_player no_of_players back name seed
	= { color = back, name = name, row = row, ligretto = ligretto, hand = { conceal = hand, discard = [] }, seed = seed }
where
	cards           = shuffle (initial_player_cards no_of_players back) seed
	(row,rest)      = splitAt (no_of_cards_in_row no_of_players) cards
	(ligretto,hand) = splitAt 10 rest

row_card :: !Int !Player -> Card
row_card row_no player=:{row}
| row_no <= 0 || row_no > length row
	= abort ("ligretto.row_card: illegal integer argument (" <+++ row_no <+++ ").\n")
| otherwise
	= row !! (row_no-1)

move_ligretto_card_to_row :: !Int !Player -> Player
move_ligretto_card_to_row row_no player=:{row,ligretto}
| row_no <= 0 || row_no > length row
	= abort ("ligretto.move_ligretto_card_to_row: illegal integer argument (" <+++ row_no <+++ ").\n")
| isEmpty ligretto
	= abort "ligretto.move_ligretto_card_to_row: trying to take card from empty ligretto.\n"
| otherwise
	= {player & row = updateAt (row_no-1) (hd ligretto) row, ligretto = tl ligretto}

top_discard :: !Player -> Maybe Card
top_discard {hand={discard}}
| isEmpty discard   = Nothing
| otherwise         = Just (hd discard)

shuffle_hand :: !Player -> Player
shuffle_hand player=:{hand=hand=:{conceal,discard},seed}
| isEmpty conceal   = {player & hand = { hand & conceal = shuffle discard r1
                                              , discard = []
                                       }
                              , seed = r2
                      }
| otherwise         = abort ("ligretto.shuffle_hand: not allowed to shuffle non-empty concealed pile.\n")
where
	[r1,r2:_]		= genRandInt (abs seed)

remove_top_of_discard :: !Player -> Player
remove_top_of_discard player=:{hand=hand=:{conceal,discard}}
| isEmpty discard   = abort ("ligretto.remove_top_of_discard: no discarded card to pick.\n")
| otherwise         = {player & hand = { hand & discard = tl discard }}

swap_discards :: !Player -> Player
swap_discards player=:{hand=hand=:{conceal,discard}}
| isEmpty conceal   = abort ("ligretto:swap_discards: not allowed to take cards from an empty conceal pile.\n")
| otherwise         = { player & hand = { hand & conceal = rest
                                               , discard = reverse top3 ++ discard
                      }                 }
where
	(top3,rest)     = splitAt 3 conceal

card_matches_top_of_pile :: !Card !Pile -> Bool
card_matches_top_of_pile card pile
| isEmpty pile			= card.no == 1
| otherwise				= let top_card = hd pile in
						  card.front === top_card.front && card.no == top_card.no+1

matching_piles :: !Card !Middle -> [(Int,Pile)]
matching_piles card middle
	= [(pileno,pile) \\ pile <- middle & pileno <- [0..] | card_matches_top_of_pile card pile]

and_the_winner_is :: !GameSt -> Maybe Player
and_the_winner_is {players}
	= case [player \\ player=:{ligretto} <- players | isEmpty ligretto] of
	    [p : _] = Just p
	    _       = Nothing
