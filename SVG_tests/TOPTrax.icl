module TOPTrax

// compile with Clean 2.4 + environment iTasks
import StdMisc
//import GenPrint
//import trax_without_generics
import trax
//import game2
import iTasks, MultiUser
import Data.List
from StdFunc import flip

derive class iTask TraxSt
derive gEditor    Coordinate, TraxTile, TileEdge, Trax
derive gEditMeta  Coordinate, TraxTile, TileEdge, Trax
derive gUpdate    Coordinate, TraxTile, TileEdge, Trax
derive gVerify    Coordinate, TraxTile, TileEdge, Trax
derive gText      Coordinate, TraxTile, TileEdge, Trax, LineColor
derive JSONEncode Coordinate, TraxTile, TileEdge, Trax
derive JSONDecode Coordinate, TraxTile, TileEdge, Trax
derive gDefault   Coordinate, TraxTile, TileEdge, Trax

Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow "SVG Trax" "Play SVG trax" play_trax
								  ] [] world

import iTasks.API.Extensions.SVG.SVGlet


:: TraxSt
 = { trax   :: Trax              // the current set of placed tiles
   , names  :: [User]            // the current two players
   , turn   :: Bool
   , choice :: Maybe Coordinate
   }

play_trax :: Task User
play_trax
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Trax with:" [] users
	  >>= \you -> play_game me you {trax=zero,names=[me,you],turn=True,choice=Nothing}

play_game :: User User TraxSt -> Task User
play_game me you traxSt
	= withShared traxSt
	  (\share -> (me @: (   updateSharedInformation (toString me  +++ " plays with red")   
	                                             [imageUpdate id (toImage True) (\_ _ -> Nothing) (flip const)] share
	                    >>* [OnValue (ifValue game_over game_winner)]
	             ))
	             -&&-
	             (you @:(   updateSharedInformation (toString you +++ " plays with white")
	                                             [imageUpdate id (toImage False) (\_ _ -> Nothing) (flip const)] share
	                    >>* [OnValue (ifValue game_over game_winner)]
	             ))
	  ) @ fst

game_over :: TraxSt -> Bool
game_over st=:{trax}
	= not (isEmpty winners)
where
	winners = loops trax ++ winning_lines trax

game_winner :: TraxSt -> Task User
game_winner st=:{trax,turn,names=[me,you]}
	= viewInformation "The winner is:" [] winner
	    -&&- 
	  viewInformation "Final board:" [imageView (toImage False) (\_ _ -> Nothing)] st @ fst
where
	winners				= loops trax ++ winning_lines trax
	prev_player_color	= if turn WhiteLine RedLine
	winner				= if (isMember prev_player_color (map fst winners)) (if turn you me) (if turn me you)

start_with_this :: TraxTile TraxSt -> TraxSt
start_with_this tile st=:{trax,turn}
	= {st & trax = add_tile zero tile trax, turn = not turn}

setcell :: Coordinate TraxSt -> TraxSt
setcell coord st
	= {st & choice = Just coord}

settile :: Coordinate TraxTile TraxSt -> TraxSt
settile coord tile st=:{trax,turn}
	= {st & trax = mandatory_moves (add_tile coord tile trax) coord, choice = Nothing, turn = not turn}

toImage :: Bool TraxSt *TagSource -> Image TraxSt
toImage my_turn st=:{trax,names=[me,you],turn} _
	= above (repeat AtMiddleX) [] [text font message, board it_is_my_turn d st] Nothing
where
	it_is_my_turn				= my_turn == turn
	message						= if it_is_my_turn "Select a tile" "Wait for other player..."
	d							= px 50.0

board :: Bool Span TraxSt -> Image TraxSt
board it_is_my_turn d st=:{trax}
| nr_of_tiles trax == zero
	| it_is_my_turn				= grid (Rows 2) (RowMajor, LeftToRight, TopToBottom) [] [] 
							           [tileImage d tile <@< {onclick = const (start_with_this tile), local = False} \\ tile <- gFDomain{|*|}] Nothing
	| otherwise					= voidImage d
| otherwise						= grid (Rows (maxy-miny+3)) (RowMajor, LeftToRight, TopToBottom) (repeat (AtMiddleX,AtMiddleY)) []
							           [  case tile_at trax coord of
							                 Nothing   = if (it_is_my_turn && isMember coord free_coords) (freeImage d coord st) (voidImage d)
							                 Just tile = tileImage d tile
							           \\ row <- [miny-1..maxy+1]
							            , col <- [minx-1..maxx+1]
							            , let coord = fromTuple (col,row)
							           ] Nothing
where
	((minx,maxx),(miny,maxy))	= bounds trax
	free_coords					= free_coordinates trax

voidImage :: Span -> Image a
voidImage d				= empty d d

freeImage :: Span Coordinate TraxSt -> Image TraxSt
freeImage d coord {trax,choice}
| maybe True (\c -> coord <> c) choice
						= unselected <@< {onclick = const (setcell coord), local = False}
| otherwise				= above [] [] [tileImage (d /. nr_of_candidates) tile <@< {onclick = const (settile coord tile), local = False} \\ tile <- candidates] Nothing
where
	candidates			= possible_tiles (linecolors trax coord)
	nr_of_candidates	= length candidates
	unselected			= tileShape d <@< {fill = toSVGColor "lightgrey"}

tileImage :: Span TraxTile -> Image a
tileImage d tile		= fromJust (lookup tile [ (horizontal,rotate (deg 0.0)   horizontal_tile)
			                                    , (vertical,  rotate (deg 90.0)  horizontal_tile)
			                                    , (northwest, rotate (deg 0.0)   northwest_tile)
			                                    , (northeast, rotate (deg 90.0 ) northwest_tile)
			                                    , (southeast, rotate (deg 180.0) northwest_tile)
			                                    , (southwest, rotate (deg 270.0) northwest_tile)
			                                    ])
where
	brick				= Just (tileShape d <@< {stroke = toSVGColor "white"} <@< {strokewidth = d /. 20})
	horizontal_tile		= overlay (repeat (AtMiddleX,AtMiddleY)) [] [ bar yline "white", bar xline "red" ] brick
	northwest_tile		= (overlay [] [(d /. 2, d /. 2),(d /. -2, d /. -2)] [ arc "white", arc "red" ] brick) <@< { MaskAttr | mask = tileShape d <@< {fill = toSVGColor "white"}}
	bar line c			= line Nothing d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5}
	arc c				= circle d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5} <@< {fill = toSVGColor "none"}

tileShape :: Span -> Image a
tileShape d				= rect d d <@< {xradius = d /. 10} <@< {yradius = d /. 10}

font	= { fontfamily  = "Arial"
	      , fontysize   = 14.0
	      , fontstretch = ""
	      , fontstyle   = ""
	      , fontvariant = ""
	      , fontweight  = ""
	      }
