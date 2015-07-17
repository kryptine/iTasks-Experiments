definition module trax_without_generics

import StdClass, StdTupleExt
import StdMaybe
import GenFDomain
import iTasks

derive class iTask LineColor, Coordinate, Trax, TraxTile, TileEdge

:: TraxTile                         // a tile connects two edges:
	= { end1 :: !TileEdge           //    the red line at one end and
	  , end2 :: !TileEdge           //    the red line at the other end
	  }
derive   gFDomain  TraxTile
instance fromTuple TileEdge TileEdge TraxTile
instance toTuple   TileEdge TileEdge TraxTile
instance ==        TraxTile
instance toString  TraxTile

horizontal :: TraxTile              // tile with a straight horizontal red line
vertical   :: TraxTile              // tile with a straight vertical   red line
northwest  :: TraxTile              // tile with an elbow red line at north-west
northeast  :: TraxTile              // tile with an elbow red line at north-east
southeast  :: TraxTile              // tile with an elbow red line at south-east
southwest  :: TraxTile              // tile with an elbow red line at south-west

/** other_edge @tile @edge = @edge`:
        the tile connects @edge and @edge`.
*/
other_edge :: !TraxTile !TileEdge -> TileEdge

:: TileEdge                         // an edge is either at:
	= North                         //    the north side of a tile, or at
	| East                          //    the east side of a tile, or at
	| South                         //    the south side of a tile, or at
	| West                          //    the west side of a tile
derive   gFDomain TileEdge
instance ==       TileEdge
instance <        TileEdge
instance ~        TileEdge

:: LineColor                        // a line color is either:
	= RedLine                       //    red, or
	| WhiteLine                     //    white
derive   gFDomain LineColor
instance toString LineColor
instance ==       LineColor
instance ~        LineColor

:: Coordinate                       // a coordinate consists of:
 = { col :: !Int                    //   a column-coordinate
   , row :: !Int                    //   a row-coordinate
   }
instance ==        Coordinate
instance <         Coordinate
instance zero      Coordinate
instance toString  Coordinate
instance fromTuple Int Int Coordinate
instance toTuple   Int Int Coordinate

:: Trax
instance ==   Trax
instance zero Trax

/** tiles @trax = @tiles`:
	   @tiles` is a finite map of all current tiles of @trax.
*/
tiles :: !Trax -> [(Coordinate,TraxTile)]

/** nr_of_tiles @trax = @nr_of_tiles:
        returns the current number of tiles (@nr_of_tiles) in @trax.
*/
nr_of_tiles :: !Trax -> Int

/** bounds @trax = ((@minx,@maxx),(@miny,@maxy)):
        returns the mimimum x-coordinate @minx and minimum y-coordinate @miny
        and the maximum x-coordinate @maxx and maximum y-coordinate @maxy of @trax.
        It is assumed that (nr_of_tiles @trax > 0).
*/
bounds :: !Trax -> (!(!Int,!Int), !(!Int,!Int))

/** dimension @trax = (@nr_of_cols,@nr_of_rows):
       returns the @nr_of_cols and @nr_of_rows of the collection of @trax.
       It is assumed that (nr_of_tiles @trax > 0).
*/
dimension :: !Trax -> (!Int,!Int)

/** add_tile @coordinate @tile @trax = @trax`:
        only if (tile_at @trax @coordinate) = Nothing and linecolors_match (linecolors @trax @coordinate) (tilecolors @tile)
        then (@coordinate,@tile) is added to @trax, resulting in @trax`.
        In any other case, @trax` = @trax.
*/
add_tile :: !Coordinate !TraxTile !Trax -> Trax

/** tile_at @trax @coordinate = Nothing:
       when no tile is present at @coordinate in @trax.
    tile_at @trax @coordinate = Just @t:
       returns tile @t which is present at @coordinate in @trax.
*/
tile_at :: !Trax !Coordinate -> Maybe TraxTile

/** free_coordinates @trax = @free:
       computes the coordinates in which a new tile can be placed.
       These coordinates are all free direct neighbours of all tiles in @trax.
*/
free_coordinates :: !Trax -> [Coordinate]

:: LineColors

/** linecolors @trax @coordinate = @colors:
       computes of a potential tile at @coordinate in @trax the corresponding @colors of the line-endings.
       tile_at @trax @coordinate should be Nothing.
*/
linecolors :: !Trax !Coordinate -> LineColors

/** possible_tiles @colors = @trax:
       returns those @trax that match with @colors.
*/
possible_tiles :: !LineColors -> [TraxTile]

:: Line

/** is_loop @path = True:
       holds only if @path is a closed loop.
    is_loop @path = False:
       @path is not a closed loop.
*/
is_loop :: !Line -> Bool

/** cut_loop @path = @path`:
       turns the infinite @path, forming a loop, into a finite @path` that contains all tiles.
*/
cut_loop :: !Line -> Line

/** loops @trax = Nothing:
       @trax contains no loop of RedLine or WhiteLine.
    loops @trax = @loops:
       @trax contains @loops, each indicating their color and path.
*/
loops :: !Trax -> [(LineColor,Line)]

/** mandatory_moves @trax @coordinate = @trax`:
       assumes that the tile at @coordinate in @trax is the most recently placed tile.
       It performs the mandatory moves that require filling empty places next to this
       tile, and all subsequent other empty places, thus resulting in @trax`.
*/
mandatory_moves :: !Trax !Coordinate -> Trax

/** winning_lines @trax = @lines:
       returns all winning @lines that start either at the west or north edge of @trax.
*/
winning_lines :: !Trax -> [(LineColor,Line)]
