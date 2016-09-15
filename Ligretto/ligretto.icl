implementation module ligretto

from   StdMisc import abort
import iTasks
import Math.Random		// for generating random numbers

:: NrOfPlayers :== Int                            // 2 upto 4 players
:: Card          = { back     :: Color            // the backside color (belonging to player)
                   , front    :: Color            // the frontside color (for playing)
                   , nr       :: Int              // 1 upto 10
                   }
:: SideUp        = Front | Back                   // the side of the card that faces upwards
:: Color         = Red | Green | Blue | Yellow    // the four player / card colors
:: Player        = { color     :: Color           // the backside color of this player's cards
                   , row       :: CardRow             // the row of the player (nr_of_cards_in_row nr_of_players)
                   , ligretto  :: Pile            // the ligretto pile: 10 upto 0 (win) cards
                   , hand      :: Hand            // the hand cards
                   }
:: Pile        :== [Card]                         // a pile of cards
:: CardRow         :== [Card]                         // a row of (nr_of_cards_in_row nr_of_players) cards
:: Hand          = { conceal   :: Pile            // the concealed pile
                   , discard   :: Pile            // the discarded pile
                   }
:: RowNr       :== Int                            // row cards are numbered 1 .. (nr_of_cards_in_row nr_of_players)
:: Seed        :== Int                            // not part of the model, but required for generating random numbers

derive gEq Card, SideUp, Color

nr_of_cards_in_row		:: !NrOfPlayers -> Int
nr_of_cards_in_row 2	= 5
nr_of_cards_in_row 3	= 4
nr_of_cards_in_row 4	= 3
nr_of_cards_in_row n	= abort ("ligretto.nr_of_cards_in_row: illegal integer argument (" +++ toString n +++ ").\n")

all_colors				:: [Color]
all_colors				= [Red,Green,Blue,Yellow]

colors :: !NrOfPlayers -> [Color]
colors nr_of_players	= take nr_of_players all_colors

initial_player_cards	:: !NrOfPlayers !Color -> Pile
initial_player_cards nr_of_players back
	= [{back=back,front=color,nr=nr} \\ color <- all_colors, nr <- [1..10]]

shuffle					:: ![a] !Seed -> [a]
shuffle xs seed			= fst (unzip (sortBy (\(_,r1) (_,r2) -> (r1 < r2)) (zip2 xs (genRandInt (abs seed)))))

initial_player			:: !NrOfPlayers !Color !Seed -> Player
initial_player nr_of_players back seed
	= { color = back, row = row, ligretto = ligretto, hand = { conceal = hand, discard = [] } }
where
	cards				= shuffle (initial_player_cards nr_of_players back) seed
	(row,rest)			= splitAt (nr_of_cards_in_row nr_of_players) cards
	(ligretto,hand)		= splitAt 10 rest

row_card				:: !RowNr !Player -> Card
row_card row_nr player=:{row}
| row_nr <= 0 || row_nr > length row
						= abort ("ligretto.row_card: illegal integer argument (" <+++ row_nr <+++ ").\n")
| otherwise				= row !! (row_nr-1)

move_ligretto_card_to_row :: !RowNr !Player -> Player
move_ligretto_card_to_row row_nr player=:{row,ligretto}
| row_nr <= 0 || row_nr > length row
						= abort ("ligretto.move_ligretto_card_to_row: illegal integer argument (" <+++ row_nr <+++ ").\n")
| isEmpty ligretto		= abort "ligretto.move_ligretto_card_to_row: trying to take card from empty ligretto.\n"
| otherwise				= {player & row = updateAt (row_nr-1) (hd ligretto) row, ligretto = tl ligretto}

top_discard				:: !Player -> Maybe Card
top_discard {hand={discard}}
| isEmpty discard   	= Nothing
| otherwise         	= Just (hd discard)

shuffle_hand			:: !Seed !Player -> Player
shuffle_hand seed player=:{hand=hand=:{conceal,discard}}
| isEmpty conceal		= {player & hand = { hand & conceal = shuffle discard seed
                                                  , discard = []
                          }                }
| otherwise				= abort ("ligretto.shuffle_hand: not allowed to shuffle non-empty concealed pile.\n")

remove_top_of_discard	:: !Player -> Player
remove_top_of_discard player=:{hand=hand=:{conceal,discard}}
| isEmpty discard		= abort ("ligretto.remove_top_of_discard: no discarded card to pick.\n")
| otherwise				= {player & hand = { hand & discard = tl discard }}

swap_discards			:: !Player -> Player
swap_discards player=:{hand=hand=:{conceal,discard}}
| isEmpty conceal		= abort ("ligretto:swap_discards: not allowed to take cards from an empty conceal pile.\n")
| otherwise				= { player & hand = { hand & conceal = rest
                                                   , discard = reverse top3 ++ discard
                          }                 }
where
	(top3,rest)			= splitAt 3 conceal

card_matches_top_of_pile:: !Card !Pile -> Bool
card_matches_top_of_pile card pile
| isEmpty pile			= card.nr == 1
| otherwise				= let top_card = hd pile in
						  card.front === top_card.front && card.nr == top_card.nr+1
