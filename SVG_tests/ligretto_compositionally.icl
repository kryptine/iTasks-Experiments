module ligretto_compositionally

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import ligrettoModel

derive class iTask GameSt, Card, Color, Player, Hand, SideUp

Start :: *World -> *World
Start world = startEngine viewAll world

// (viewInformation "Ligretto step by step" [imageView steps] ()) world
viewAll :: Task [((), GameSt)]
viewAll = allTasks (map f imgs)
  where
  f :: (Image (), Image GameSt) -> Task ((), GameSt)
  f (txt, img) = viewInformation "Code" [imageView (\_ _ -> txt) (\_ _ -> Nothing)] () -&&- viewInformation "Image" [imageView (\_ _ -> img) (\_ _ -> Nothing)] game_state

//steps :: () -> Image ()
//steps _
	//= grid (Rows (length imgs / 2)) (LeftToRight,TopToBottom) (repeat (AtLeft,AtTop)) []
		 //(map (margin hspace) imgs) Nothing

imgs :: [(Image (), Image GameSt)]
imgs = [(lines ["card_width    = px 58.5"
               ,"card_height   = px 90.0"
               ,"card_shape    = rect card_width card_height"
               ,"                   <@< {xradius = card_height /. 18}"
               ,"                   <@< {yradius = card_height /. 18}"
               ], mkImg card_shape)
       ,(lines ["big_no 7 Red"], mkImg (big_no 7 Red))
       ,(lines ["ligretto Red"], mkImg (ligretto Red))
       ,(lines ["card      = {back = Red, front = Green, no = 7}"
               ,"card_image Front card"], mkImg (card_image Front card))
       ,(lines ["card_image Back card"], mkImg (card_image Back  card))
       ,(lines ["empty_card    = card_shape <@< {fill = toSVGColor \"lightgrey\"}"
               ,"no_card_image = overlay [(AtMiddleX,AtMiddleY)]"
               ,"                 [] [text (pilefont 12.0) \"empty\"]"
               ,"                 (Just empty_card)"
               ], mkImg no_card_image)
       ,(lines ["pile_of_cards Front [{front = fromInt i, back = Red, no = i} \\  i <- [1..6]]"
              ], mkImg (margin card_height a_pile))
       ,(lines ["pile_image Front [{front = fromInt i, back = Red, no = i} \\ i <- [1..24]]"
              ], mkImg (margin card_height a_pile`))
       ,(lines ["name_image alice"
              ], mkImg (name_image alice))
       ,(lines ["middle_image  (card_height *. 2) (repeatn 12 [])"], mkImg (margin card_height (middle_image  (card_height *. 2) (repeatn 12 []))))
       ,(lines ["south_player"], mkImg (margin card_height (player_image (card_height *. 4) False south_player)))
       ]

south_player  = { color    = Red
                , name     = "alice"
                , row      = [card 10 Red Red,card 8 Red Green,card 7 Red Yellow,card 10 Red Blue]
                , ligretto = [card 4 Red Red, card 1 Red Green, card 1 Red Yellow, card 1 Red Yellow, card 1 Red Blue, card 1 Red Red, card 1 Red Green, card 1 Red Blue, card 1 Red Yellow, card 1 Red Yellow]
                , hand     = { conceal = repeatn 26 (card 1 Red Green), discard = [] }
                , seed     = 42
                }
where
	card x b f= {back=b,front=f,no=x}

// all definitions below originate from or are specializations of TOPLigretto:
hspace        = px 2.0
font          = normalFontDef "Courier New" 9.0
card          = {back = Red, front = Green, no = 7}

game_state    = { middle = [], players = [] }





/*

dragTest = updateInformation "test" [imageUpdate id (\st -> pict st) (\st v -> v)] NoClick
where
  pict st = beside [AtTop,AtTop] [] [clicks (showDrag st card_shape) , showDrag st card_shape] Nothing

derive class iTask DragStatus

:: DragStatus = NoClick | Clicked | DragStart | DragEnd | DragEnter | DragOver

showDrag DragStart image      = image <@< {fill = toSVGColor Red}
showDrag DragEnd image      = image <@< {fill = toSVGColor Green}
showDrag DragEnter image      = image <@< {fill = toSVGColor Blue}
showDrag DragOver image      = image <@< {fill = toSVGColor Yellow}
showDrag Clicked image        = image <@< {fill = toSVGColor "darkgrey"}
showDrag _ image              = image <@< {fill = toSVGColor "grey"}


clicks image = image <@< {onclick         = (\st -> Clicked)}
                   <@< {ondragstart     = (\st -> DragStart)}
//                     <@< {ondragend     = (\st -> DragEnd)}
//                     <@< {ondragenter     = (\st -> DragEnter)}
//                     <@< {ondragover     = (\st -> DragOver)}



*/



instance fromInt Color where
	fromInt n = [Red,Green,Blue,Yellow] !! ((abs n) rem 4)

mkImg img = margin (px 10.0) (above (repeat AtLeft) [] [img] Nothing)

lines txt = above (repeat AtLeft) [] (map (text font) txt) Nothing

a_pile   = pile_of_cards Front [{front = fromInt i, back = Red, no = i} \\  i <- [1..6]]
a_pile`  = pile_image Front [{front = fromInt i, back = Red, no = i} \\ i <- [1..24]]

alice = {color = Red, name = "alice", row = [], ligretto = [], hand = {conceal = [], discard = []}, seed = 42}

// Image definitions:
// these have been taken from a 'real' physical card game of Ligretto, dimensions to be interpreted as mm
card_width  :== px 58.5
card_height :== px 90.0

// frequently occurring constants:
white       :== toSVGColor "white"
black       :== toSVGColor "black"

//card_shape :: Image m
card_shape  :== rect card_width card_height <@< {xradius = card_height /. 18} <@< {yradius = card_height /. 18}

//no_card_image :: Image m
no_card_image   :== overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] (Just (card_shape <@< {fill = toSVGColor "lightgrey"}))

big_no no color :== text (cardfont 20.0) (toString no) <@< {fill   = white}
                                                       <@< {stroke = toSVGColor color}
ligretto  color :== text (cardfont 12.0) "Ligretto"    <@< {fill   = toSVGColor "none"}
                                                       <@< {stroke = toSVGColor color}

card_image :: !SideUp !Card -> Image m
card_image side card
  #! host = Just (card_shape <@< {fill = if (side === Front) (toSVGColor card.front) white})
  | side === Front
     #! no = margin (px 5.0)
               (big_no card.no (no_stroke_color card.front))
     = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [no, rotate (deg 180.0) no] host
  | otherwise
     = overlay [(AtMiddleX,AtBottom)] [] [skewy (deg -20.0) (ligretto card.back)] host
  where
  no_stroke_color :: !Color -> Color
  no_stroke_color Red    = Blue
  no_stroke_color Green  = Red
  no_stroke_color Blue   = Yellow // Green gives better visual results, but yellow is 'official'
  no_stroke_color Yellow = Green

pile_of_cards :: !SideUp !Pile -> Image m
pile_of_cards side pile
  = overlay [] [(zero,card_height /. 18 *. dy) \\ dy <- [0 .. ]]
               (map (card_image side) (reverse pile)) (Just no_card_image)

pile_image :: !SideUp !Pile -> Image m
pile_image side pile
  #! no_of_cards     = length pile
  #! top_cards       = take 10 pile
  #! top_cards_image = pile_of_cards side top_cards
  | no_of_cards > 10 = above [AtMiddleX] [] [text (pilefont 10.0) (toString no_of_cards),top_cards_image] Nothing
  | otherwise        = top_cards_image

row_images :: !Bool !RowPlayer -> [Image GameSt]
row_images interactive row
  = [  tuneIf interactive (card_image Front row_card)
              {onclick = play_row_card row_card.back no}
	\\ row_card <- row 
	 & no       <- [1..]
	]

hand_images :: !Bool !Hand !Color -> [Image GameSt]
hand_images interactive {conceal,discard} color
  #! conceal_pile = pile_image Back  conceal
  #! discard_pile = pile_image Front discard
  = [ tuneIf interactive conceal_pile {onclick = play_concealed_pile color}
    , tuneIf interactive discard_pile {onclick = play_hand_card color}
    ]

player_arc :== 0.45 * pi

player_image :: !Span !Bool !Player -> Image GameSt
player_image r interactive player
  = circular r player_arc
               (  row_images interactive player.row
               ++ [pile_image Front player.ligretto]
               ++ hand_images interactive player.hand player.color
               )

players_image :: !Span !Color ![Player] -> Image GameSt
players_image r color players
  #! no = length players
  = rotate (rad (player_arc/(toReal (2*(3+no_of_cards_in_row no))) - player_arc/2.0)) 
           (circular zero (2.0*pi)
                [  player_image r (player.color === color) player 
                \\ player <- players
                ]
           )

name_image :: !Player -> Image m
name_image {name,color}
 = overlay [(AtMiddleX,AtMiddleY)] []
     [text {cardfont 16.0 & fontweight = "bold"} name <@< {fill = if (color === Yellow) black white}]
     (Just (rect width height <@< {fill = toSVGColor color}))
     <@< {mask = rect width height <@< {fill = white} <@< {stroke = white}}
where
	width  = card_height *. 1.8
	height = card_width  *. 0.4

names_image :: !Span ![Player] -> Image m
names_image r players = circular r (2.0*pi) (map name_image players)

//middle_image :: !Span !Middle -> Image m
middle_image r middle :== circular r (2.0*pi) (map (pile_image Front) middle)

player_perspective :: !Color !GameSt *[*(ImageTag, *ImageTag)] -> Image GameSt
player_perspective color gameSt=:{players} _
  #! angle = 2.0*pi / (toReal (length players))
  #! my_no = hd [i \\ player <- players & i <- [0..] | player.color === color]
  = rotate (rad (~(toReal my_no*angle))) (game_image color gameSt)

game_image :: !Color !GameSt -> Image GameSt
game_image color {players,middle}
  = overlay (repeat (AtMiddleX,AtMiddleY)) []
            ([ middle_image  (card_height *. 2) middle          // inner-most tier: the middle cards
             , names_image   (card_height *. 3.2) players       // the middle tier: the player names
             , players_image (card_height *. 4) color players   // outer-most tier: the player cards
             ]
            ) (Just (empty (card_height *. 12) (card_height *. 12)))

//	a generally useful image combinator:
circular :: !Span !Real ![Image m] -> Image m
circular r a imgs
  #! n      = length imgs
  #! sign_a = toReal (sign a)
  #! a`     = normalize (rad a)
  #! alpha  = (toRad a`) / (toReal n)
  = overlay (repeat (AtMiddleX,AtMiddleY))
                    [(~r *. cos angle,~r *. sin angle) \\ i <- [0.0, sign_a ..], angle <- [i*alpha - 0.5*pi]]
                    [rotate (rad (i*alpha)) img \\ i <- [0.0, sign_a ..] & img <- imgs]
                    (Just (empty (r *. 2) (r *. 2)))              // BUG: using Nothing creates incorrect image (offset to left)

pi =: 3.14159265359

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"

//cardfont :: !Real -> FontDef
cardfont size :== normalFontDef "Verdana" size

//pilefont :: !Real -> FontDef
pilefont size :== normalFontDef "Verdana" size
