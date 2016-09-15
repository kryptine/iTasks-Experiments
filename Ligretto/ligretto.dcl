definition module ligretto
 
import iTasks

/** A model for the game of Ligretto.
*/
:: NrOfPlayers :== Int                            // 2 upto 4 players
:: Middle      :== [Pile]                         // 0 upto 16 piles
:: Pile        :== [Card]                         // 0 upto 10 cards of same front color
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
:: CardRow         :== [Card]                         // nr_of_cards_in_row nr_of_players cards
:: Hand          = { conceal   :: Pile            // the concealed pile
                   , discard   :: Pile            // the discarded pile
                   }
:: RowNr       :== Int                            // row cards are numbered 1 .. (nr_of_cards_in_row nr_of_players)
:: Seed        :== Int                            // not part of the model, but required for generating random numbers

/** nr_of_cards_in_row nr_of_players = n:
        @n is the number of cards that a row should have.
        @nr_of_players should be one of: 2, 3, 4.
*/
nr_of_cards_in_row   :: !NrOfPlayers -> Int

/** colors nr_of_players = player_colors:
        @player_colors are the colors in a game consisting of @nr_of_players players.
        @nr_of_players should be one of: 2, 3, 4.
*/
colors               :: !NrOfPlayers -> [Color]

/** initial_player nr_of_players color n = player:
        @player is a player with all cards shuffled and distributed to the @player.row, @player.ligretto, and @player.hand.
        @nr_of_players should be one of: 2, 3, 4.
        @color should be one of: (colors @nr_ofplayers).
*/
initial_player       :: !NrOfPlayers !Color !Seed -> Player

/** row_card row_nr player = card:
		@card is the card at @row_nr in the current @player.row (counted as 1..(nr_of_cards_in_row nr_of_players)).
		This function aborts if @row_nr is not one of these values.
*/
row_card             :: !RowNr !Player -> Card

/** move_ligretto_card_to_row row_nr player = player`:
        @card is the card at @row_nr in the current @player.row (counted as 1..nr_of_players).
        The current card at @row_nr in the current @player.row (counted as 1..(nr_of_cards_in_row nr_of_players))
        is replaced by the current top card in @player.ligretto, which therefor contains one card less.
        This function aborts if @player.ligretto is empty.
*/
move_ligretto_card_to_row :: !RowNr !Player -> Player

/** top_discard player = Nothing:
        @player has no current cards in her @player.hand.discard pile.
    top_discard player = Just card:
        @player has @card on top of her @player.hand.discard pile.
*/
top_discard          :: !Player -> Maybe Card

/** shuffle_hand seed player = player`:
        shuffles the current @player.hand.discard pile and sets it to @player`.hand.conceal.
        This function aborts if @player.hand.conceal is empty.
*/
shuffle_hand         :: !Seed !Player -> Player

/** remove_top_of_discard player = player`:
        @player` is identical to @player except that the card at the top of @player.hand.discard is removed.
        This function aborts if @player.hand.discard is not empty.
*/
remove_top_of_discard:: !Player -> Player

/** swap_discards player = player`:
        moves the top 3 cards from the @player.hand.conceal pile in reverse order to the @player.hand.discard pile.
        @player.hand.conceal must not be empty.
*/
swap_discards        :: !Player -> Player

/** card_matches_top_of_pile card pile = match:
		@match is True only if @pile is empty and @card.nr is 1 or if the front color of
		the card on top of @pile is equal to the front color of @card and the value of the
		the card on top of @pile is one less than the value of @card.
*/
card_matches_top_of_pile :: !Card !Pile -> Bool
