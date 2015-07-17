module gui

import StdBool, StdFunc, StdInt, StdListExt, StdMisc, StdStringExt, StdTupleExt
import StdIO
import Notice
import draw, trax

:: GameSt
 = { trax   :: !Trax              // the current set of placed tiles
   , free   :: !Maybe Coordinate  // a free location chosen by the user
   , ids    :: !GameIds           // the changeable components of the game
   , names  :: !Players           // the current two players
   , turn   :: !LineColor         // the color of the player at turn
   }
:: GameIds
 = { traxId :: !Id                // the Id of the trax control
   , nameId :: !Id                // the Id of the name tag of the current player
   , turnId :: !Id                // the Id of the turn-indicator
   , tileIds:: ![(Tile,Id)]       // the Tile-Id pairs of the tile buttons
   , nameIds:: !(!Id,!Id)         // the Ids of the red / white player text fields
   }

:: Players                        // the players of a game of trax are:
 = { red    :: !Name              // the red player is starting
   , white  :: !Name              // the white player
   }
:: Name
 :== String

name_of :: !Players !LineColor -> Name
name_of players RedLine = players.red
name_of players white   = players.white

openGameIds :: !*env -> (!GameIds,!*env) | Ids env
openGameIds env
	= case openIds 11 env of
	    ([traxId,nameId,turnId,horId,verId,nwId,neId,seId,swId,redId,whiteId], env)
	      = ({ traxId = traxId
	         , nameId = nameId
	         , turnId = turnId
	         , tileIds = [ (horizontal,horId)
	                     , (vertical,  verId)
	                     , (northwest, nwId )
	                     , (northeast, neId )
	                     , (southeast, seId )
	                     , (southwest, swId )
	                     ]
	         , nameIds = (redId,whiteId)
	         }, env)
	    _ = abort "openGameIds: could not create identification values for the GUI components."

initGameSt :: !GameIds !Players -> GameSt
initGameSt ids names
	= { trax = zero
	  , free = Nothing
	  , ids  = ids
	  , names= names
	  , turn = RedLine
	  }

getPlayerNames :: !(PSt GameSt) -> PSt GameSt
getPlayerNames pSt=:{ls=gameSt}
# (okId,pSt)			= openId pSt
# dialog				= Dialog "Player Names"
							(   EditControl names.red   width 1 [ControlId redId,  ControlPos (Right,zero)] :+: TextControl "Red:"   [ControlPos (LeftOfPrev,zero)]
							:+: EditControl names.white width 1 [ControlId whiteId,ControlPos (Right,zero)] :+: TextControl "White:" [ControlPos (LeftOfPrev,zero)]
							:+: ButtonControl "Ok" [ControlId okId,ControlFunction (noLS (confirmF names)),ControlPos (Right,zero)]
							)
							[ WindowOk okId ]
= snd (openModalDialog undef dialog pSt)
where
	names				= gameSt.names
	nameId				= gameSt.ids.nameId
	(redId,whiteId)		= gameSt.ids.nameIds
	width				= ContentWidth "MMMMMMMMMMMMMMMMMMMMM"
	
	confirmF :: !Players !(PSt GameSt) -> PSt GameSt
	confirmF old_names pSt=:{ls=gameSt}
	# (mWSt,pSt)		= accPIO (getParentWindow redId) pSt
	| isNothing mWSt	= pSt
	# wSt				= fromJust mWSt
	# new_names			= case map (fromJust o snd) (getControlTexts [redId,whiteId] wSt) of
						     [red,white] = {red=red,white=white}
						     _           = old_names
	# pSt				= {pSt & ls={gameSt & names=new_names}}
	# pSt				= closeActiveWindow pSt
	# pSt				= appPIO (setControlText nameId new_names.red) pSt
	= pSt

Start :: *World -> *World
Start world
	# (ids,world)		= openGameIds world
	= startIO SDI (initGameSt ids {red="red",white="white"}) (startTrax o getPlayerNames) [ProcessClose closeProcess] world

startTrax :: !(PSt GameSt) -> PSt GameSt
startTrax pSt=:{ls=gameSt}
	# window			= Window "Trax" 
						     (   trax_control gameSt {w=w-2*bw,h=h} [ControlPos (LeftTop,zero)]
						     :+: LayoutControl 
						     (   turn_control gameSt {w=tw,h=th} [ControlPos (Left,zero)]
						     :+: TextControl gameSt.names.red [ControlId gameSt.ids.nameId,ControlPos (RightToPrev,zero),ControlWidth (PixelWidth (2*bw+hspace-tw))]
						     :+: ListLS
						         [   tile_button leftTile  {w=bw,h=bh} [ControlPos (Left,       zero) : map ControlId (lookup leftTile  gameSt.ids.tileIds)]
						         :+: tile_button rightTile {w=bw,h=bh} [ControlPos (RightToPrev,zero) : map ControlId (lookup rightTile gameSt.ids.tileIds)]
						         \\ (leftTile,rightTile) <- [(horizontal,vertical)
						                                    ,(northwest, northeast)
						                                    ,(southwest, southeast)
						                                    ]
						         ]
						     )   [ControlPos (RightTop,zero)
						         ,ControlItemSpace hspace vspace
						         ]
						     )
						     [ WindowClose    (noLS closeProcess)
						     , WindowViewSize {w=w,h=h}
						     ]
	# (error,pSt)		= openWindow Void window pSt
	| error <> NoError	= abort (toString error)
	| otherwise			= pSt
	where
		(w, h)			= (600,600)
		(bw,bh)			= ( 40, 40)
		(tw,th)			= ( 18, 18)
		(hspace,vspace)	= (  1,  1)

game_over :: !(PSt GameSt) -> PSt GameSt
game_over pSt=:{ls=gameSt=:{trax,names,turn}}
| not (isEmpty lines)
	= openNotice notice pSt
| otherwise
	= pSt
where
	lines		= loops trax ++ winning_lines trax
	other_player= ~turn
	win_player	= if (isMember other_player (map fst lines)) other_player turn
	type		= if (isEmpty (winning_lines trax)) "loop" "line"
	notice		= Notice ["Congratulations, " <+ name_of names win_player <+ "!" 
				         ,"You've created the first " <+ type <+ "."
				         ,""
				         ,"Do you want to play another game?"
				         ]
				         (NoticeButton "Yes" (noLS new_game))
				         [NoticeButton "No"  (noLS closeProcess)]

new_game :: !(PSt GameSt) -> PSt GameSt
new_game pSt
# pSt			= getPlayerNames pSt
# (gameSt,pSt)	= accPLoc (\gameSt -> let new = {gameSt & trax=zero,free=Nothing,turn=RedLine} in (new,new)) pSt
# pSt			= appPIO ((switch_turn gameSt) o (show_trax gameSt) o (switch_tile_buttons gameSt)) pSt
= pSt

tile_button :: !Tile !Size ![ControlAttribute *(.ls,PSt GameSt)] -> CustomButtonControl .ls (PSt GameSt)
tile_button tile size atts
	= CustomButtonControl size (\enabled _ -> draw_sized size zero {tile=tile,enabled=enabled}) [ControlFunction (noLS buttonF) : atts]
where
	buttonF :: !(PSt GameSt) -> PSt GameSt
	buttonF pSt=:{ls=gameSt=:{trax,free,ids,turn},io}
	| nr_of_tiles trax == 0
		# gameSt= {gameSt & trax=add_tile zero tile trax, free=Nothing, turn= ~turn}
		# io	= show_trax           gameSt io
		# io	= switch_tile_buttons gameSt io
		# io	= switch_player       gameSt io
		# io	= switch_turn         gameSt io
		# pSt	= {pSt & ls=gameSt, io=io}
		= pSt
	| isNothing free
		= pSt
	| otherwise
		# trax	= add_tile new tile trax
		# trax	= mandatory_moves trax new
		# gameSt= {gameSt & trax=trax, free=Nothing, turn= ~turn}
		# io	= show_trax           gameSt io
		# io	= switch_tile_buttons gameSt io
		# io	= switch_player       gameSt io
		# io	= switch_turn         gameSt io
		# pSt	= {pSt & ls=gameSt, io=io}
		= game_over pSt
	where
		new		= fromJust free

switch_tile_buttons :: !GameSt -> IdFun *(IOSt .ls)
switch_tile_buttons gameSt=:{trax,free,ids}
| nr_of_tiles trax == 0
	= (enableControls (map snd ids.tileIds))
| otherwise
	= (enableControls (map snd jay)) o (disableControls (map snd nay))
where
	matches		= case free of
				    Nothing  = []
				    Just new = possible_tiles (linecolors trax new)
	(jay,nay)	= spanfilter (\(tile,_) -> isMember tile matches) ids.tileIds

switch_player :: !GameSt -> IdFun *(IOSt .ls)
switch_player gameSt=:{ids,names,turn}
	= setControlText ids.nameId (if (turn == RedLine) names.red names.white)

turn_control :: !GameSt !Size ![ControlAttribute *(.ls,PSt GameSt)] -> CustomControl .ls (PSt GameSt)
turn_control gameSt size atts
	= CustomControl size (turn_look gameSt)
	    [ ControlId gameSt.ids.turnId
	    : atts
	    ]

turn_look :: !GameSt SelectState !UpdateState !*Picture -> *Picture
turn_look gameSt=:{turn} _ {newFrame} picture
# picture	= setPenColour Black picture
# picture	= fill         newFrame picture
# picture	= setPenColour (if (turn == RedLine) Red White) picture
# picture	= fillAt       {x=w/2+1,y=h/2} {oval_rx=w/2-2,oval_ry=h/2-2} picture
= picture
where
	{w,h}	= rectangleSize newFrame

switch_turn :: !GameSt -> IdFun *(IOSt .ls)
switch_turn gameSt=:{ids,turn}
	= setControlLook ids.turnId True (True,turn_look gameSt)

trax_control :: !GameSt !Size ![ControlAttribute *(.ls,PSt GameSt)] -> CustomControl .ls (PSt GameSt)
trax_control gameSt size atts
	= CustomControl size (trax_look gameSt)
		[ ControlResize (\oldCSize oldWSize newWSize -> {w=oldCSize.w+newWSize.w-oldWSize.w,h=oldCSize.h+newWSize.h-oldWSize.h})
		, ControlMouse  onlyMouseDown Able (noLS1 mouseF)
		, ControlId     gameSt.ids.traxId
		: atts
		]
where
	onlyMouseDown :: !MouseState -> Bool
	onlyMouseDown (MouseDown _ _ _)	= True
	onlyMouseDown _					= False
	
	mouseF :: !MouseState !(PSt GameSt) -> PSt GameSt
	mouseF (MouseDown pos _ _) pSt=:{ls=gameSt=:{trax,ids},io}
	| nr_of_tiles trax == 0	= pSt
	# (mWSt,io)				= getParentWindow traxId io
	| isNothing mWSt		= {pSt & io=io}
	# wSt					= fromJust mWSt
	# (_,viewSize)			= getControlViewSize traxId wSt
	# tilesize				= tile_size gameSt viewSize
	# coordinate			= toCoordinate (minx,miny) tilesize pos
	# is_free				= isMember coordinate frees
	# free_coordinate		= if is_free (Just coordinate) Nothing
	# gameSt				= {gameSt & free = free_coordinate}
	# io					= show_trax           gameSt io
	# io					= switch_tile_buttons gameSt io
	# pSt					= {pSt & ls = gameSt, io = io}
	= pSt
	where
		frees				= free_coordinates trax
		((minx,_),(miny,_))	= bounds trax
		traxId				= ids.traxId
		tileIds				= ids.tileIds
	mouseF _ pSt
		= pSt

show_trax :: !GameSt -> IdFun *(IOSt .ls)
show_trax gameSt=:{ids}
	= setControlLook ids.traxId True (True,trax_look gameSt)

toCoordinate :: !(!Int,!Int) !Size !Point2 -> Coordinate
toCoordinate (minx,miny) tilesize pos
	= {col = pos.x / tilesize.w + minx - 1, row = pos.y / tilesize.h + miny - 1}

fromCoordinate :: !(!Int,!Int) !Size !Coordinate -> Point2
fromCoordinate (minx,miny) tilesize coordinate
	= {x=(coordinate.col-minx+1)*tilesize.w,y=(coordinate.row-miny+1)*tilesize.h}

trax_look :: !GameSt SelectState !UpdateState !*Picture -> *Picture
trax_look gameSt=:{trax,free} selectSt updSt=:{newFrame} picture
# picture				= unfill newFrame picture
| nr_of_tiles trax == 0
	# (w,picture)		= getPenFontStringWidth msg picture
	# picture			= drawAt {x=(wsize.w-w)/2,y=wsize.h/2} msg picture
	= picture
	with msg			= "Press any of the buttons to start playing"
# picture				= foldl draw_tile picture (tiles trax)
| isNothing free
	= picture
| otherwise
	# picture			= draw (tile_rect (minx,miny) tilesize (fromJust free)) picture
	= picture
where
	wsize				= rectangleSize newFrame
	(dw,dh)				= dimension trax
	((minx,_),(miny,_))	= bounds trax
	tilesize			= tile_size gameSt (rectangleSize newFrame)

	draw_tile :: !*Picture !(!Coordinate,!Tile) -> *Picture
	draw_tile picture (coord,tile)
		= draw_sized tilesize (fromCoordinate (minx,miny) tilesize coord) tile picture
	
	tile_rect :: !(!Int,!Int) !Size !Coordinate -> Rectangle
	tile_rect (minx,miny) tilesize coordinate
		= {corner1=lefttop, corner2=movePoint (toVector tilesize) lefttop}
	where
		lefttop			= fromCoordinate (minx,miny) tilesize coordinate

tile_size :: !GameSt !Size -> Size
tile_size gameSt=:{trax} size
	= {w=d, h=d}
where
	(dw,dh)				= dimension trax
	(w,h)				= (size.w / (dw+2), size.h / (dh+2))
	d					= max 10 (min w h)

instance draw_sized Tile where
	draw_sized size pos tile picture
		= appPicture (draw_tile bright size pos tile) picture
instance draw_sized TileButton where
	draw_sized size pos {tile,enabled} picture
		= appPicture (draw_tile (if (enabled == Able) bright pale) size pos tile) picture

draw_tile :: !TileColors !Size !Point2 !Tile -> *Picture -> *Picture
draw_tile colours size=:{w,h} pos=:{x,y} tile
	= seq [ setPenColour colours.background
          , fill         rect
          , setPenSize   linewidth
          , setPenColour colours.whiteline
          , white
          , setPenColour colours.redline
          , red
          , setPenSize   1
          , setPenColour LightGrey
          , draw         rect
          ]
where
	rect			= {corner1=pos, corner2=movePoint (toVector size) pos}
	region			= toRegion rect
	linewidth		= min (w/5) (h/5)
	(mw,mh)			= (w/2, h/2)
	(horz,vert)		= ( {corner1={pos & y=y+mh-linewidth/2},corner2={x=x+w,y=y+mh+linewidth/2}}
					  , {corner1={pos & x=x+mw-linewidth/2},corner2={x=x+mw+linewidth/2,y=y+h}}
					  )
	oval			= {oval_rx=mw+linewidth/2,oval_ry=mh+linewidth/2}
	(nw,ne,se,sw)	= ( ({x=x+mw+linewidth/2,y=y  },{curve_oval = oval, curve_from = 0.0, curve_to = 1.5*PI, curve_clockwise = True })
					  , ({x=x+mw-linewidth/2,y=y  },{curve_oval = oval, curve_from = PI,  curve_to = 1.5*PI, curve_clockwise = False})
					  , ({x=x+mw-linewidth/2,y=y+h},{curve_oval = oval, curve_from = PI,  curve_to = 0.5*PI, curve_clockwise = True })
					  , ({x=x+mw+linewidth/2,y=y+h},{curve_oval = oval, curve_from = 0.0, curve_to = 0.5*PI, curve_clockwise = False})
					  )
	(red,white)		= hd (lookup tile [(horizontal,(fill horz, fill vert))
					                  ,(vertical,  (fill vert, fill horz))
					                  ,(northwest, (appClipPicture region (uncurry drawAt nw), appClipPicture region (uncurry drawAt se)))
					                  ,(northeast, (appClipPicture region (uncurry drawAt ne), appClipPicture region (uncurry drawAt sw)))
					                  ,(southeast, (appClipPicture region (uncurry drawAt se), appClipPicture region (uncurry drawAt nw)))
					                  ,(southwest, (appClipPicture region (uncurry drawAt sw), appClipPicture region (uncurry drawAt ne)))
					                  ])
		
::  TileButton
 =	{ tile       :: !Tile
    , enabled    :: !SelectState
    }
::	TileColors
 =	{ background :: !Colour
 	, redline    :: !Colour
 	, whiteline  :: !Colour
 	}

bright :: TileColors
bright = { background = Black
         , redline    = Red
         , whiteline  = White
         }

pale   :: TileColors
pale   = { background = DarkGrey
         , redline    = RGB {r=MaxRGB,g=80,b=80}
         , whiteline  = LightGrey
         }
