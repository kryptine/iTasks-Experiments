implementation module top

// compile with Clean 2.4 + environment iTasks
import StdListExt, StdMisc
import GenPrint
import trax_without_generics
//import trax
//import game2
import gameN

derive class iTask TraxSt
/*derive gEditor    Coordinate, Tile, Edge, Trax
derive gEditMeta  Coordinate, Tile, Edge, Trax
derive gUpdate    Coordinate, Tile, Edge, Trax
derive gVerify    Coordinate, Tile, Edge, Trax
derive gText      Coordinate, Tile, Edge, Trax
derive JSONEncode Coordinate, Tile, Edge, Trax
derive JSONDecode Coordinate, Tile, Edge, Trax
derive gDefault   Coordinate, Tile, Edge, Trax */

toLineColor :: !Turn -> LineColor
toLineColor turn = if (match 0 turn) RedLine WhiteLine

player :: ![User] !Turn -> User
player [a,b] turn = if (match 0 turn) a b

:: TraxSt
 = { trax   :: !Trax              // the current set of placed tiles
   , names  :: ![User]            // the current two players
   }

play_trax :: Task Turn
play_trax = play_for_N 2 { game   = "Trax"
                         , state  = initial_state
                         , over   = game_over
                         , winner = declare_winner
                         , move   = make_a_move
                         , board  = show_board
                         }

initial_state :: ![User] -> TraxSt
initial_state users = { trax = zero, names = users }

game_over :: !(Turn,TraxSt) -> Bool
game_over (_,traxSt) = not (isEmpty (loops traxSt.trax ++ winning_lines traxSt.trax))

declare_winner :: (Turn,TraxSt) -> Task Turn
declare_winner (turn,traxSt=:{trax,names})
	= viewInformation "The winner is:" [ViewWith (toString o (player names))] winner
where
	winners        = loops trax ++ winning_lines trax
	last_player    = prev turn
	winner         = if (isMember (toLineColor last_player) (map fst winners)) last_player turn

make_a_move :: (Turn,TraxSt) -> Task TraxSt
make_a_move (turn,traxSt=:{trax})
	=              chooseCoordinate trax
      >>= \new  -> chooseTile   new trax
      >>= \tile -> return {traxSt & trax = mandatory_moves (add_tile new tile trax) new}
where
	chooseCoordinate :: !Trax -> Task Coordinate
	chooseCoordinate trax
	| nr_of_tiles trax == 0    = return zero
	| otherwise                = enterChoice "Choose coordinate:" [ChooseWith (ChooseFromComboBox toString)] (free_coordinates trax)
	
	chooseTile :: !Coordinate !Trax -> Task Tile
	chooseTile coordinate trax = enterChoice "Choose tile:" [ChooseWith (ChooseFromRadioButtons (TileTag (16,16)))] 
		                             (if (nr_of_tiles trax == 0) 
		                                 gFDomain{|*|} 
		                                 (possible_tiles (linecolors trax coordinate))
		                             )

show_board :: !(Turn,TraxSt) -> [HtmlTag]
show_board (turn,traxSt=:{trax,names})
| nr_of_tiles trax == 0 = [h3 ("Select any tile, " <+++ current_player)]
| otherwise				= [h3 current_player, board]
where
	board				= TableTag [BorderAttr "0"] 
						           [ tr [  cell {col=minx+x-1,row=miny+y-1} 
						                \\ x <- [0..nrcol+1]
						                ] 
						           \\ y <- [0..nrrow+1]
						           ]
	current_player      = player names turn
    cell coordinate     = case tile_at trax coordinate of
                             Nothing   = if (isMember coordinate free) (text coordinate) (text "")
                             Just tile = td [TileTag (42,42) tile]
    
    free                      = free_coordinates trax
    ((minx,maxx),(miny,maxy)) = bounds           trax
    (nrcol,nrrow)             = dimension        trax

TileTag :: !(!Int,!Int) !Tile -> HtmlTag
TileTag (width,height) tile
	= ImgTag [SrcAttr ("/" <+++ toString tile <+++ ".png"),WidthAttr (toString width),HeightAttr (toString height)]

// shorthands for HTML:
tr		= TrTag []
td		= TdTag []
text x	= TdTag [AlignAttr "center"] [Text (toString x)]
h3   x	= H3Tag [] [text x]
