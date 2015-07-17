module TicTacTop4

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import iTasks._Framework.Tonic
import TaskLayout

// Tic-Tac-Toe entities:
:: Game       = { board   :: Board
                , players :: Players
                , turn    :: TicTac
                }
:: Players    = { tic     :: User
                , tac     :: User 
                }
:: Name     :== String
:: Board    :== [[Tile]]
:: Tile     :== Maybe TicTac
:: TicTac     = Tic | Tac
:: Coordinate = { col :: Int, row :: Int }             

// Tic-Tac-Toe relations:
initial_game :: Players -> Game
initial_game players = { board   = repeatn 3 (repeatn 3 Nothing)
                       , players = players
                       , turn    = Tic
                       }

name_of :: TicTac Players -> Name
name_of Tic players = toString players.tic
name_of Tac players = toString players.tac

game_over :: Game -> Bool
game_over game = it_is_a_draw game || isJust (winner game)

it_is_a_draw :: Game -> Bool
it_is_a_draw game = all isJust (flatten game.board) && isNothing (winner game)

winner :: Game -> Maybe TicTac
winner game
	= hd ([hd nodups \\ candidate <- rows ++ columns ++ diags
	                  , nodups    <- [removeDup candidate]
	                  | length nodups == 1 && isJust (hd nodups)
	      ] ++ [Nothing])
where
	rows	= game.board
	columns	= [[ game.board !! y !! x \\ y <- [0..2]] \\ x <- [0..2]]
	diags   = [[row !! i \\ row <- game.board & i <- [0..]], [row !! (2-i) \\ row <- game.board & i <- [0..]]]

move :: Coordinate Game -> Game
move new game
	= {game & board = updateAt new.row (updateAt new.col (Just game.turn) (game.board!!new.row)) game.board, turn = ~game.turn}

// Tic-Tac-Toe coordination:
shared_game :: Players -> Shared Game
shared_game players
	= sharedStore ("TicTacTop: " <+++ name_of Tic players <+++ " vs " <+++ name_of Tac players) game
where
	game = initial_game players

find_players :: Task Players
find_players
	=           get currentUser
    >>= \me  -> updateChoiceWithShared "Find player" [] users me
    >>= \you -> return {tic=me,tac=you}

//  variant 4: all board cells, player name, player glyph are separate tasks, one locally known shared state:
play_game :: TicTac (Shared Game) -> Task Game
play_game who shared_game
	=            get shared_game 
	>>= \game -> let player = if (who === Tic) game.players.tic game.players.tac in
	             player @: (
	                  getTags 11 >>= \[t1,t2:ts] -> 
	                  anyTask [ tagTask t1 (viewSharedInformation () [ViewWith  (\game -> name_of game.turn game.players)] shared_game)
	                          , tagTask t2 (viewSharedInformation () [imageView (\game _ -> glyph game.turn) handleConflict] shared_game)
	                          : [  tagTask t (edit_cell who {col=i,row=j} shared_game)
	                            \\ t <- ts & (i,j) <- [(i,j) \\ i <- [0 .. 2], j <- [0 .. 2]]
	                            ]
	                          ] <<@ ArrangeTaskUI (\taskUI -> Beside (repeatn 3 AtMiddleY) 
	                                                            [ withLayout (Grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) (repeatn 9 (AtMiddleX,AtMiddleY)) ts)
	                                                            , t1,t2
	                                                            ])
	             >>* [OnValue (ifValue game_over (accolades who))]
	             )
where
	toViewModel		    = id
	fromViewModel  _	= id
	handleConflict _ _	= Nothing

edit_cell :: TicTac Coordinate (Shared Game) -> Task Game
edit_cell who coord shared_game = updateSharedInformation () [imageUpdate toViewModel (\game _ -> render who coord game) handleConflict fromViewModel] shared_game
where
	toViewModel        = id
	fromViewModel  _   = id
	handleConflict _ _ = Nothing
	
	render who coord game	= tuneIf (isNothing c && who === game.turn) (cell c) {onclick = const (move coord), local = False}
	where c					= game.board!!coord.row!!coord.col

accolades :: TicTac Game -> Task Game
accolades me game
	= (case winner game of
	    Just wins    = if (wins == me)
	                      (viewInformation "Game over" [] "I won!")
	                      (viewInformation "Game over" [] "I lost!")
	    no_winner    = if (it_is_a_draw game)
	                      (viewInformation "No winner" [] "It is a draw")
	                      (viewInformation "Keep playing" [] "Game isn't over yet")
	  ) @! game

play_games :: (Shared Game) -> Task ()
play_games shared_game
	=   upd (\game -> {Game | game & board = repeatn 3 (repeatn 3 Nothing), turn = Tic}) shared_game
	>>| ((play_game Tic shared_game -|| play_game Tac shared_game)
	>>* [ OnAction (Action "/File/New"  [])          (always (play_games shared_game))
        , OnAction (Action "/File/Quit" [ActionIcon "quit"]) (always (return ()))
        ])

play_tic_tac_toe :: Task ()
play_tic_tac_toe
	=               find_players 
	>>= \players -> withShared (initial_game players) play_games

// Tic-Tac-Toe rendering:
render :: TicTac Game -> Image Game
render playing game = board (playing === game.turn) game.board

board :: Bool Board -> Image Game
board am_playing board
	= grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtMiddleY)) []
          [  tuneIf (isNothing c && am_playing)
                    (cell c) {onclick = const (move {row=i,col=j}), local = False}
          \\ row <- board 
           & i   <- [0 ..]
           , c   <- row
           & j   <- [0 ..]
          ] Nothing

cell :: (Maybe TicTac) -> Image a
cell Nothing		= rect (px edge) (px edge) <@< {fill = toSVGColor "white"}
cell (Just tictac)	= glyph tictac

glyph :: TicTac -> Image a
glyph Tic			= circle (px (edge - pencil - 3.0)) <@< {fill = toSVGColor "none"} <@< {strokewidth = px pencil}
glyph Tac			= overlay (repeat (AtMiddleX,AtMiddleY)) [] 
					     [  line Nothing slash (px edge) (px edge) <@< {strokewidth = px pencil} 
					     \\ slash <- [Slash,Backslash]
					     ]  Nothing

edge   = 30.0
pencil = edge / 5.0
font   = normalFontDef "Times New Roman" edge

// The iTask entry code:
import MultiUser	// for working with multiply registered users

Start :: *World -> *World
Start world
	= StartMultiUserTasks
	              [workflow "Tic-Tac-Top" "Play Tic-Tac-Toe" (play_tic_tac_toe                         <<@ FullScreen)]
	              [publish "/users" (WebApp []) (const (set_up_users                                   <<@ FullScreen))
	              ,publish "/show"  (WebApp []) (const (viewSharedInformation "Current users" [] users <<@ FullScreen))
	              ,publish "/tonic" (WebApp []) (const (tonicStaticBrowser []                          <<@ FullScreen))
	              ] world

derive class iTask Game, Players, TicTac
instance == TicTac where == t1 t2 = t1 === t2
instance ~  TicTac where ~ Tic = Tac
                         ~ Tac = Tic

// For TFP demonstration purposes only:
set_up_users :: Task ()
set_up_users =           get userAccounts
             >>= \all -> set (foldl (\xs x -> removeDup [x:xs]) all new) userAccounts
             >>| return ()
where
	new = [{UserAccount | credentials = { username = Username "peter", password = Password "peter" }
	                    , title       = Nothing
	                    , roles       = ["tic-tac-toe player"]
	       }
	      ,{UserAccount | credentials = { username = Username "pieter", password = Password "pieter" }
	                    , title       = Nothing
	                    , roles       = ["tic-tac-toe player"]
	       }
	      ,{UserAccount | credentials = { username = Username "jurrien", password = Password "jurrien" }
	                    , title       = Nothing
	                    , roles       = ["tic-tac-toe player"]
	       }
	      ]
instance == UserAccount where == u1 u2 = u1 === u2
