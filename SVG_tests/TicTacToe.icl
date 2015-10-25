module TicTacToe

import iTasks
import MultiUser



Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow  "Original Tic-Tac-Toe" "Play tic-tac-toe"  	 tictactoe
								  , workflow  "SVG Tic-Tac-Toe"      "Play SVG tic-tac-toe"  tictactoe2
								  ] [] world

// tic-tac-toe, simplistic

:: TicTacToe
	= { board   :: ![[Maybe TicTac]]
	  , player1 :: !User
	  , player2 :: !User
	  , turn    :: !Bool               // player 1 is playing
	  }
:: TicTac
	= Tic | Tac

instance ~ Bool where ~ b = not b
derive class iTask TicTacToe, TicTac
instance == TicTac where == t1 t2 = t1 === t2

// ttt utility functions

emptyBoard :: [[Maybe TicTac]]
emptyBoard = repeatn 3 (repeatn 3 Nothing)

add_cell :: !(!Int,!Int) !Bool ![[Maybe TicTac]] -> [[Maybe TicTac]]
add_cell new turn board
	= [ [  if (new == (x,y)) (Just (if turn Tic Tac)) cell
	    \\ cell <- row & x <- [1..]
	    ]
	  \\ row <- board & y <- [1..]
	  ]

game_over :: TicTacToe -> Bool
game_over {board}
	= not (isEmpty [hd nodups \\ candidate <- rows ++ columns ++ diags
	                           , nodups    <- [removeDup candidate]
	                           | length nodups == 1 && isJust (hd nodups)
	               ])
where
	rows	= board
	columns	= [[ board !! x !! y \\ x <- [0..2]] \\ y <- [0..2]]
	diags   = [[row !! i \\ row <- board & i <- [0..]], [row !! (2-i) \\ row <- board & i <- [0..]]]


on_turn :: Bool TicTacToe -> Bool
on_turn my_turn gameSt=:{turn}
	= turn == my_turn

free_coordinates :: [[Maybe a]] -> [(Int,Int)]
free_coordinates board
	= map fst (filter (isNothing o snd) (flatten [[((x,y),cell) \\ cell <- row & x <- [1..]] \\ row <- board & y <- [1..]]))

// ttt game playing

tictactoe :: Task String
tictactoe
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Tic-Tac-Toe with:" [] users
	  >>= \you -> playGame me you {board=emptyBoard,player1=me,player2=you,turn=True}
where
	playGame me you board 
	  =				withShared  board
	                         (\sharedGameSt ->
	                         (        (tictactoe_for_1 False me you sharedGameSt)
	                                       -||-
	                          (you @: (tictactoe_for_1 True you me sharedGameSt))
	                         ))
	  >>|			playGame me you {board	& turn = not board.turn}	

tictactoe_for_1 :: !Bool !User !User !(Shared TicTacToe) -> Task User
tictactoe_for_1 my_turn me you sharedGameSt
	= (viewSharedInformation ("Turn = " <+++ if my_turn me you  <+++ ", Board =") [ViewWith (\gameSt -> viewBoard (42,42) gameSt)] sharedGameSt) ||- play
where
	play= (updateSharedInformation "Play:" [UpdateWith Hidden (\gameSt _ -> gameSt)] sharedGameSt)
	      >>* [ OnValue (ifValue game_over declare_winner)
              , OnValue (ifValue (on_turn my_turn)  make_a_move)
              ]

	make_a_move gameSt=:{board,turn}
		=              enterChoice "Choose coordinate:" [] (free_coordinates board)
		  >>= \new  -> let board`  = add_cell new turn board
		                   gameSt` = {gameSt & board = board`
		                                     , turn = ~turn
		                             }
		                in set gameSt` sharedGameSt >>| play

declare_winner gameSt
	= 	let winner = if gameSt.turn gameSt.player2 gameSt.player1 
		in 		viewInformation "And the winner is: " [] (toString winner)
			>>|	return winner


// showing the gameboard in Html 

viewBoard :: !(!Int,!Int) !TicTacToe -> HtmlTag
viewBoard dimensions ttt
	= TableTag [ BorderAttr "0" ] 
	           [ tr [ td [TileTag dimensions (case cell of
												Nothing  = "empty"
												Just Tic = "cross"
												Just Tac = "circle")
						 ]
					\\ cell <- row
					] 
			    \\ row <- ttt.board
			    ]

// shorthands for HTML:
tr		= TrTag []
td		= TdTag []
//text x	= TdTag [AlignAttr "center"] [Text (toString x)]

TileTag :: !(!Int,!Int) !String -> HtmlTag
TileTag (width,height) tile
	= ImgTag [SrcAttr ("/"<+++ tile <+++ ".png"),w,h]
where
	(w,h) = (WidthAttr (toString width),HeightAttr (toString height))

// SVG variant

import iTasks.API.Extensions.SVG.SVGlet

tictactoe2 :: Task User
tictactoe2
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Tic-Tac-Toe with:" [] users
	  >>= \you -> playGame2 me you {board=emptyBoard,player1=me,player2=you,turn=True}

playGame2 :: User User TicTacToe -> Task User
playGame2 me you ttt 
	= withShared ttt
		(\share ->  updateSharedInformation (toString me) [imageUpdate toAction (toImage True) (\_ _ -> Nothing) fromAction] share 
					-||
//					(you @: updateSharedInformation (toString you) [imageUpdate toAction (toImage False) fromAction] share)  // not working
					updateSharedInformation (toString you) [imageUpdate toAction (toImage False) (\_ _ -> Nothing) fromAction] share
		) 
		>>* [ OnValue (ifValue game_over declare_winner)
		  	]

toAction :: TicTacToe -> ActionState (Int,Int) TicTacToe
toAction ttt = {ActionState | state = ttt, action = Nothing} 

fromAction :: TicTacToe (ActionState (Int,Int) TicTacToe) -> TicTacToe 
fromAction _ {ActionState|state = ttt, action = Just (i,j)} 
	= {ttt & turn 	= not ttt.turn
		   , board	= add_cell (i,j) ttt.turn ttt.board
	}
fromAction _ as = as.ActionState.state

toImage ::  Bool (ActionState (Int,Int) TicTacToe) *TagSource -> Image (ActionState (Int,Int) TicTacToe)
toImage turn ttt _
	= grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) [] [] 
		[ text ArialRegular12px showTurn
		, text ArialRegular12px ""
		, tttBoard
		] Nothing
where	
	myTurn 	 = turn == ttt.ActionState.state.turn 
	showTurn = if myTurn "play: it is your turn..." "wait: it is not your turn..." 
	tttBoard = grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) [] [] 
	       		[ mkTile x y myTurn cell 
	       			\\ row <- ttt.ActionState.state.board & y <- [1..3], cell <- row & x <- [1..3] 
	      		] Nothing

mkTile i j _ (Just Tic)   = cross
mkTile i j _ (Just Tac)   = null
mkTile i j True Nothing   = blank <@< {onclick = \_ st -> {st & ActionState.action = Just (i,j)}, local = False}
mkTile i j False Nothing  = blank 

cross = overlay [] [] [blank,bar Slash,bar Backslash] Nothing <@< {mask = emptyTile}
where
	bar dir = line Nothing dir (px 30.0) (px 30.0) <@< {strokewidth = px 5.0} <@< {stroke = SVGColorText "red" }

null  = overlay [] [] [blank,naught] Nothing
where
	naught = circle (px 30.0) <@< {fill = SVGColorText "white"} <@< {stroke = toSVGColor "blue"} <@< {mask = circle (px 30.0) <@< {fill = toSVGColor "white"} <@< {strokewidth = zero }}

emptyTile 	= rect (px 30.0) (px 30.0) <@< {fill = SVGColorText "white"}
blank 		= emptyTile <@< {stroke = SVGColorText "black"} <@< {strokewidth = px 1.0} <@< {fill = toSVGColor "white"}

ArialRegular12px :== { fontfamily  = "Arial"
                     , fontysize   = 12.0
                     , fontstretch = "normal"
                     , fontstyle   = "normal"
                     , fontvariant = "normal"
                     , fontweight  = "normal"
                     }
