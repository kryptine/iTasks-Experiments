definition module ligrettoModel
 
import Data.Maybe

/** A model for the game of Ligretto.
*/
:: NoOfPlayers :== Int                            // 2 upto 4 players
:: Middle      :== [Pile]                         // 0 upto 16 piles
:: Pile        :== [Card]                         // 0 upto 10 cards of same front color
:: Card          = { back     :: Color            // the backside color (belonging to player)
                   , front    :: Color            // the frontside color (for playing)
                   , no       :: Int              // 1 upto 10
                   }
:: SideUp        = Front | Back                   // the side of the card that faces upwards
:: Color         = Red | Green | Blue | Yellow    // the four player / card colors
:: Player        = { color     :: Color           // the backside color of this player's cards
                   , name      :: String          // the name of the player
                   , row       :: RowPlayer       // the row of the player (no_of_cards_in_row no_of_players)
                   , ligretto  :: Pile            // the ligretto pile: 10 upto 0 (win) cards
                   , hand      :: Hand            // the hand cards
                   , seed      :: Int             // the random seed necessary for shuffling cards
                   }
:: RowPlayer   :== [Card]                         // no_of_cards_in_row no_of_players cards
:: Hand          = { conceal   :: Pile            // the concealed pile
                   , discard   :: Pile            // the discarded pile
                   }
:: GameSt        = { middle    :: !Middle         // all middle cards
                   , players   :: ![Player]       // all players
                   }

//	Game state functions:

/** play_concealed_pile player game = game`:
		if the concealed pile of @player in @game is empty, then all cards on the discard pile
		get shuffled and moved to the concealed pile in @game`.
		if the concealed pile of @player in @game is not empty, then the top three cards of the
		concealed pile are moved to the discard pile in @game`.
*/
play_concealed_pile  :: !Color !GameSt -> GameSt

/** play_hand_card player game = game`:
		the top card of the discard pile of @player in @game is placed on an available middle pile
		in @game` if possible, otherwise @game` is equal to @game.
*/
play_hand_card       :: !Color !GameSt -> GameSt

/** play_row_card player no game = game`:
		moves the card at row @no of @player in @game to an available middle pile in @game` if
		possible, otherwise @game` is equal to @game.
*/
play_row_card        :: !Color !Int !GameSt -> GameSt

/** get_player color game = player:
		@player is the player in @game identified by @color.
*/
get_player           :: !Color !GameSt -> Player

/** colors no_of_players = player_colors:
        @player_colors are the colors in a game consisting of @no_of_players players.
        @no_of_players should be one of: 2, 3, 4.
*/
colors               :: !NoOfPlayers -> [Color]

/** no_of_cards_in_row no_of_players = n:
		@no_of_players should be one of: 2, 3, 4.
		Depending on this value, @n is the number of cards in the 'row' of a player (decreases with increasing
		@no_of_players).
*/
no_of_cards_in_row   :: !NoOfPlayers -> Int

/** initial_player no_of_players color name n = player:
        @player is a player with all cards shuffled and distributed to the @player.row, @player.ligretto, and @player.hand.
        @no_of_players should be one of: 2, 3, 4.
        @color should be one of: (colors @no_ofplayers).
        @name is the preferably short name of the player
*/
initial_player       :: !NoOfPlayers !Color !String !Int -> Player

/** and_the_winner_is game = maybe_player:
		if there is a player with an empty Ligretto pile, then @maybe_player = Just player; 
		otherwise @maybe_player = Nothing.
*/
and_the_winner_is    :: !GameSt -> Maybe Player
