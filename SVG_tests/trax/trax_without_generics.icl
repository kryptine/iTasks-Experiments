implementation module trax_without_generics

// version without proper generics because otherwise iTask won't compile (what?!?)
import StdBool, StdListExt, StdMisc, StdOrdListExt, StdTupleExt
from   StdFunc import flip
import StdMaybe
import GenFDomain
import iTasks

derive class iTask LineColor, Coordinate, Trax, TraxTile, TileEdge
derive bimap []

:: TraxTile                         // a tile connects two edges:
	= { end1 :: !TileEdge           //    the red line at one end and
	  , end2 :: !TileEdge           //    the red line at the other end
	  }
gFDomain{|TraxTile|} = map fromTuple [(West,East),(North,South),(North,West),(North,East),(South,East),(South,West)]
instance fromTuple TileEdge TileEdge TraxTile where fromTuple (e1,e2) = {end1 = e1, end2 = e2}
instance toTuple   TileEdge TileEdge TraxTile where toTuple   tile    = (tile.end1, tile.end2)
instance == TraxTile where == {end1=a1,end2=a2} {end1=b1,end2=b2} = (a1,a2) == (b1,b2) || (a2,a1) == (b1,b2)
instance toString TraxTile where
	toString tile = lookup1 tile [(horizontal,"horizontal")
	                             ,(vertical,  "vertical"  )
	                             ,(northwest, "northwest" )
	                             ,(northeast, "northeast" )
	                             ,(southeast, "southeast" )
	                             ,(southwest, "southwest" )
	                             ]

horizontal :: TraxTile
horizontal =: fromTuple (West,East)

vertical :: TraxTile
vertical =: fromTuple (North,South)

northwest :: TraxTile
northwest =: fromTuple (North,West)

northeast :: TraxTile
northeast =: fromTuple (North,East)

southeast :: TraxTile
southeast =: fromTuple (South,East)

southwest :: TraxTile
southwest =: fromTuple (South,West)

other_edge :: !TraxTile !TileEdge -> TileEdge
other_edge tile edge = if (edge == tile.end1) tile.end2 tile.end1

:: TileEdge                         // an edge is either at:
	= North                         //    the north side of a tile, or at
	| East                          //    the east side of a tile, or at
	| South                         //    the south side of a tile, or at
	| West                          //    the west side of a tile
derive gFDomain TileEdge
instance toInt  TileEdge where toInt North = 0
                               toInt East  = 1
                               toInt South = 2
                               toInt West  = 3
instance ==     TileEdge where == e1 e2 = toInt e1 == toInt e2
instance <      TileEdge where <  e1 e2 = toInt e1 < toInt e2
instance ~      TileEdge where ~  e     = case e of
                                             North = South
                                             South = North
                                             West  = East
                                             East  = West

:: LineColor                        // a line color is either:
	= RedLine                       //    red, or
	| WhiteLine                     //    white
derive gFDomain LineColor
instance toString LineColor where toString RedLine = "red"
                                  toString WhiteLine = "white"
instance ==       LineColor where == RedLine   RedLine   = True
                                  == WhiteLine WhiteLine = True
                                  == _         _         = False
instance ~        LineColor where ~ RedLine    = WhiteLine
                                  ~ WhiteLine  = RedLine


:: Coordinate                       // a coordinate consists of:
 = { col :: !Int                    //   a column-coordinate
   , row :: !Int                    //   a row-coordinate
   }
instance ==        Coordinate where == c1 c2 = c1.col == c2.col && c1.row == c2.row
instance <         Coordinate where <  c1 c2 = c1.col < c2.col || c1.col == c2.col && c1.row < c2.row
instance zero      Coordinate where zero     = {col=zero, row=zero}
instance toString  Coordinate where toString c = "(" <+++c.col<+++","<+++c.row<+++")"
instance fromTuple Int Int Coordinate where fromTuple (c,r)     = {col=c,row=r}
instance toTuple   Int Int Coordinate where toTuple   {col,row} = (col,row)

col :: !Coordinate -> Int
col coordinate = coordinate.col

row :: !Coordinate -> Int
row coordinate = coordinate.row

north :: !Coordinate -> Coordinate
north coordinate = {coordinate & row = coordinate.row-1}

south :: !Coordinate -> Coordinate
south coordinate = {coordinate & row = coordinate.row+1}

west :: !Coordinate -> Coordinate
west coordinate = {coordinate & col = coordinate.col-1}

east :: !Coordinate -> Coordinate
east coordinate = {coordinate & col = coordinate.col+1}

go :: !TileEdge -> Coordinate -> Coordinate
go North = north
go East  = east
go South = south
go West  = west


:: Trax                                 // a collection of tiles consists of:
 = { tiles :: ![(Coordinate,TraxTile)]  //   tiles that are placed on a certain location
   }
instance == Trax where == t1 t2 = sortBy fst_smaller t1.tiles == sortBy fst_smaller t2.tiles
instance zero Trax where zero = { tiles = [] }


/** tiles @trax = @tiles`:
	   @tiles` is a finite map of all current tiles of @trax.
*/
tiles :: !Trax -> [(Coordinate,TraxTile)]
tiles trax = trax.tiles


:: Line                             // a line is:
    :== [Coordinate]                //   a list of coordinates such that subsequent elements are immediate neighbours

minimum_winning_line_length :== 8	// the minimum length of a winning line

/** nr_of_tiles @trax = @nr_of_tiles:
        returns the current number of tiles (@nr_of_tiles) in @trax.
*/
nr_of_tiles :: !Trax -> Int
nr_of_tiles trax
	= length trax.tiles

/** bounds @trax = ((@minx,@maxx),(@miny,@maxy)):
        returns the mimimum x-coordinate @minx and minimum y-coordinate @miny
        and the maximum x-coordinate @maxx and maximum y-coordinate @maxy of @trax.
        It is assumed that (nr_of_tiles @trax > 0).
*/
bounds :: !Trax -> (!(!Int,!Int), !(!Int,!Int))
bounds trax
| nr_of_tiles trax > 0 = ((minList cols,maxList cols), (minList rows,maxList rows))
| otherwise            = abort "bounds: partial function is applied to empty set of tiles.\n"
where
	coords             = map fst trax.tiles
	cols               = map col coords
	rows               = map row coords

/** dimension @trax = (@nr_of_cols,@nr_of_rows):
       returns the @nr_of_cols and @nr_of_rows of the collection of @trax.
       It is assumed that (nr_of_tiles @trax > 0).
*/
dimension :: !Trax -> (!Int,!Int)
dimension trax
| nr_of_tiles trax > 0        = (maxx-minx+1, maxy-miny+1)
| otherwise                   = abort "dimension: partial function is applied to empty set of tiles.\n"
where
	((minx,maxx),(miny,maxy)) = bounds trax

/** add_tile @coordinate @tile @trax = @trax`:
        only if (tile_at @trax @coordinate) = Nothing and linecolors_match (linecolors @trax @coordinate) (tilecolors @tile)
        then (@coordinate,@tile) is added to @trax, resulting in @trax`.
        In any other case, @trax` = @trax.
*/
add_tile :: !Coordinate !TraxTile !Trax -> Trax
add_tile coordinate tile trax
| nr_of_tiles trax == 0 ||
  isMember coordinate (free_coordinates trax) && linecolors_match (linecolors trax coordinate) (tilecolors tile)
	= {trax & tiles = [(coordinate,tile) : trax.tiles]}
| otherwise
	= trax

/** tile_at @trax @coordinate = Nothing:
       when no tile is present at @coordinate in @trax.
    tile_at @trax @coordinate = Just @t:
       returns tile @t which is present at @coordinate in @trax.
*/
tile_at :: !Trax !Coordinate -> Maybe TraxTile
tile_at trax coordinate
	= case lookup coordinate trax.tiles of
	    [tile : _] = Just tile
	    none_found = Nothing

/** neighbours @coordinate = @coordinates:
       returns the list of @coordinates of the immediate neighbours to the north, east, south, and west of @coordinate.
*/
neighbours :: !Coordinate -> [Coordinate]
neighbours coordinate
	= map (flip go coordinate) gFDomain{|*|}

/** free_neighbours @trax @coordinate = @free:
       @free are those immediate neighbours at @coordinate that contain no tile in @trax.
*/
free_neighbours :: !Trax !Coordinate -> [Coordinate]
free_neighbours trax coordinate
	= [neighbour \\ neighbour <- neighbours coordinate | isNothing (tile_at trax neighbour)]

/** tile_neighbours @trax @coordinate = @trax:
       @trax are those immediate neighbours at @coordinate that contain a tile in @trax.
*/
tile_neighbours :: !Trax !Coordinate -> [Coordinate]
tile_neighbours trax coordinate
	= [neighbour \\ neighbour <- neighbours coordinate | isJust (tile_at trax neighbour)]

/** free_coordinates @trax = @free:
       computes the coordinates in which a new tile can be placed.
       These coordinates are all free direct neighbours of all tiles in @trax.
*/
free_coordinates :: !Trax -> [Coordinate]
free_coordinates trax
	= removeDupSortedList (sort (flatten (map (free_neighbours trax) (map fst trax.tiles))))

:: LineColors                        // linecolors contains the colors of the line-endings at the edges of a coordinate:
 :== [(TileEdge,Maybe LineColor)]    //    at each edge, the corresponding color is determined (might be not present)

linecolors_match :: !LineColors !LineColors -> Bool
linecolors_match lc1 lc2
	= and [match c1 c2 \\ (_,c1) <- sortBy fst_smaller lc1 
	                    & (_,c2) <- sortBy fst_smaller lc2
	      ]
where
	match :: !(Maybe a) !(Maybe a) -> Bool | Eq a
	match (Just c1) (Just c2) = c1 == c2
	match _         _         = True
	
fst_smaller :: !(!a,c) !(!a,d) -> Bool | Ord a
fst_smaller (a,_) (b,_)   = a < b

/** linecolors @trax @coordinate = @colors:
       computes of a potential tile at @coordinate in @trax the corresponding @colors of the line-endings.
       tile_at @trax @coordinate should be Nothing.
*/
linecolors :: !Trax !Coordinate -> LineColors
linecolors trax coordinate 
//	= [ (edge,gMap{|*->*|} (color_at_tile (~edge)) (tile_at trax (go edge coordinate)))
	= [ (edge,mapMaybe (color_at_tile (~edge)) (tile_at trax (go edge coordinate)))
	  \\ edge <- gFDomain{|*|}
	  ]

/** tilecolors @tile = @colors:
       returns the @colors of the line-endings of this tile.
*/
tilecolors :: !TraxTile -> LineColors
tilecolors tile
	= [(North,Just n),(East,Just e),(South,Just s),(West,Just w)]
where
	(n,e,s,w) = lookup1 tile [(horizontal,(WhiteLine,RedLine,WhiteLine,RedLine))
	                         ,(vertical,  (RedLine,WhiteLine,RedLine,WhiteLine))
	                         ,(northwest, (RedLine,WhiteLine,WhiteLine,RedLine))
	                         ,(northeast, (RedLine,RedLine,WhiteLine,WhiteLine))
	                         ,(southwest, (WhiteLine,WhiteLine,RedLine,RedLine))
	                         ,(southeast, (WhiteLine,RedLine,RedLine,WhiteLine))
	                         ]

/** color_at_tile @edge @tile = @color:
       returns the @color of the given @tile at its given @edge.
*/
color_at_tile :: !TileEdge !TraxTile -> LineColor
color_at_tile edge tile
	= fromJust (lookup1 edge (tilecolors tile))

/** possible_tiles @colors = @trax:
       returns those @trax that match with @colors.
*/
possible_tiles :: !LineColors -> [TraxTile]
possible_tiles colors
	= [tile \\ tile <- gFDomain{|*|} | linecolors_match colors (tilecolors tile)]

/** track @trax @color @edge @coordinate = @line:
       computes the entire reachable @line, starting at @coordinate in @trax, and starting
       at the given @edge.
       All tiles in the computed @line have the same @color.
*/
track :: !Trax !LineColor !TileEdge !Coordinate -> Line
track trax color edge coordinate
	= case tile_at trax coordinate of
	    Nothing   = []                    // tile at coordinate does not exist
	    Just tile = let edge` = other_edge (perspective color tile) edge
	                 in [coordinate : track trax color (~edge`) (go edge` coordinate)]

/** is_loop @path = True:
       holds only if @path is a closed loop.
    is_loop @path = False:
       @path is not a closed loop.
*/
is_loop :: !Line -> Bool
is_loop [c:cs] = isMember c cs
is_loop empty  = False

/** cut_loop @path = @path`:
       turns the infinite @path, forming a loop, into a finite @path` that contains all tiles.
*/
cut_loop :: !Line -> Line
cut_loop [c:cs] = [c : takeWhile ((<>) c) cs]

/** loops @trax = Nothing:
       @trax contains no loop of RedLine or WhiteLine.
    loops @trax = @loops:
       @trax contains @loops, each indicating their color and path.
*/
loops :: !Trax -> [(LineColor,Line)]
loops trax
	= [(RedLine,  loop) \\ loop <- color_loops trax.tiles RedLine] 
	      ++ 
	  [(WhiteLine,loop) \\ loop <- color_loops trax.tiles WhiteLine]
where
	color_loops :: ![(Coordinate,TraxTile)] !LineColor -> [Line]
	color_loops [] color	= []
	color_loops [(coordinate,tile):tiles] color
	| is_loop line			= [line : loops]
	| otherwise				= loops
	where
		line				= track trax color (start_edge tile color) coordinate
		loops				= color_loops (removeMembersBy (\(c,t) c` -> c == c`) tiles (cut_loop line)) color

/** start_edge @tile @color = @edge:
       determines at which @edge of @tile to start looking for a potential loop of @color.
*/
start_edge :: !TraxTile !LineColor -> TileEdge
start_edge tile color
	= choose (lookup1 tile [(horizontal,(West, North))
	                       ,(vertical,  (North,West ))
	                       ,(northwest, (North,South))
	                       ,(northeast, (North,South))
	                       ,(southeast, (South,North))
	                       ,(southwest, (South,North))
	                       ])
where
	choose = if (color == RedLine) fst snd

/** perspective @color @tile = @tile`:
       if @color is RedLine, then @tile` = @tile (RedLine is the default view).
       if @color is WhiteLine, then @tile` gives the point of view of the white player.
*/
perspective :: !LineColor !TraxTile -> TraxTile
perspective RedLine tile 
	= tile
perspective white tile
	= lookup1 tile [(horizontal,vertical  )
	               ,(vertical,  horizontal)
	               ,(northwest, southeast )
	               ,(northeast, southwest )
	               ,(southwest, northeast )
	               ,(southeast, northwest )
	               ]

/** winning_lines @trax = @lines:
       returns all winning @lines that start either at the west or north edge of @trax.
*/
winning_lines :: !Trax -> [(LineColor,Line)]
winning_lines trax
| nr_of_tiles trax == 0 = []
| otherwise             = winning_lines_at trax West ++ winning_lines_at trax North

/** winning_lines_at @trax @edge = @lines:
       returns all winning @lines that start at @edge in @trax.
       It is assumed that (nr_of_tiles @trax <> 0).
*/
winning_lines_at :: !Trax !TileEdge -> [(LineColor,Line)]
winning_lines_at trax edge
| max - min + 1 < minimum_winning_line_length
	= []
| otherwise
	= [  (color,line)
	  \\ (coordinate,tile) <- trax.tiles                 | min == coord coordinate
	  ,  color     <- [color_at_tile edge tile]
	  ,  line      <- [track trax color edge coordinate] | not (is_loop line)
	  ,  end       <- [last line]                        | max == coord end
	  ,  Just tile <- [tile_at trax end]                 | color_at_tile (~edge) tile == color
	  ]
where
	((minx,maxx),(miny,maxy)) = bounds trax
	(min,max,coord)           = lookup1 edge [ (West, (minx,maxx,col))
	                                         , (East, (maxx,minx,col))
	                                         , (North,(miny,maxy,row))
	                                         , (South,(maxy,miny,row))
	                                         ]

/** mandatory_tiles @trax @coordinate = @candidates:
       @candidates are those immediate, free, neighbours of the tile at @coordinate in @trax
       at which two of the same line colors end.
*/
mandatory_tiles :: !Trax !Coordinate -> [Coordinate]
mandatory_tiles trax coordinate
	= case tile_at trax coordinate of
	     Nothing = []
	     _       = [free \\ free <- free_neighbours trax coordinate
	                     |  hasDup (filter isJust (map snd (linecolors trax free)))
	               ]

/** mandatory_moves @trax @coordinate = @trax`:
       assumes that the tile at @coordinate in @trax is the most recently placed tile.
       It performs the mandatory moves that require filling empty places next to this
       tile, and all subsequent other empty places, thus resulting in @trax`.
*/
mandatory_moves :: !Trax !Coordinate -> Trax
mandatory_moves trax coordinate
| isNothing (tile_at trax coordinate)
	= abort ("mandatory_moves: a tile is expected at coordinate " +++ toString coordinate +++ "\n")
| otherwise
	= qfoldl mandatory_tiles move trax (mandatory_tiles trax coordinate)
where
	move :: !Trax !Coordinate -> Trax
	move trax filler = add_tile filler (hd (possible_tiles (linecolors trax filler))) trax
