module TOPLigretto

import ligrettoModel
import MultiUser
from   StdFunc import flip
from   StdMisc import abort
from   Control.Monad import replicateM
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

Start :: *World -> *World
//	Use this Start function to work with single user and tonic.
/*
Start world = startEngine [ publish "/" (WebApp []) (\_-> browseExamples [ workflow "SVG Ligretto" "Play SVG Ligretto" play_Ligretto])
                          , tonicViewer []] world
where
	browseExamples taskList = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (browseAuthenticated taskList))
			] )
	
	browseAuthenticated taskList {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist taskList)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	
*/
//	Use this Start function to work with multiple users and for paper screen shots.
Start world = StartMultiUserTasks [ workflow "SVG Ligretto" "Play SVG Ligretto" play_Ligretto ]
                                  [ publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                                  ] world



// this function really should be part of iTask API:
userId :: User -> UserId
userId (AuthenticatedUser id _ _) = id
userId SystemUser                 = "system-user"
userId _                          = "anonymous"



//	SVG version of Ligretto
import iTasks.API.Extensions.SVG.SVGlet

//	Make iTask infrastructure available for Ligretto model data types:
derive class iTask GameSt, Player, Color, Hand, Card, SideUp

//	Task description of Ligretto:
play_Ligretto :: Task (!Color,!String)
play_Ligretto
	=            get currentUser
	>>= \me   -> invite_friends
	>>= \them -> let us = zip2 (colors (1+length them)) [me : them]
	              in allTasks (repeatn (length us) (get randomInt))
	>>= \rs   -> let gameSt = init_gameSt us rs
	              in withShared gameSt (play_game us)

init_gameSt :: [(Color,User)] [Int] -> GameSt
init_gameSt us rs
	= { middle  = repeatn (4*length us) []
      , players = [  initial_player (length us) c (userId u) (abs r) 
                  \\ (c,u) <- us
                   & r     <- rs
                  ]
      }

invite_friends :: Task [User]
invite_friends
	=            enterSharedMultipleChoice "Select friends to play with" [] users
	>>= \them -> if (not (isMember (length them) [1..3]))
	                (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	                (return them)

play_game :: ![(Color,User)] !(Shared GameSt) -> Task (Color,String)
play_game users game_st
//	= anyTask [ (u,"Play Ligretto") @: play (c,userId u) game_st \\ (c,u) <- users ]
	= anyTask (map (play_game` game_st) users)
where
	play_game` game_st (c,u) = (u,"Play Ligretto") @: play (c,userId u) game_st

play :: !(!Color,!String) !(Shared GameSt) -> Task (Color,String)
play (me,name) game_st
    =   updateSharedInformation name [imageUpdate id (player_perspective me) (\_ _ -> Nothing) (const id)] game_st
    >>* [OnValue (withValue (\gameSt -> determine_winner gameSt
                            >>= \winner -> return (accolades winner me game_st >>| return winner)))]
    //>>* [OnValue (withValue (game_over color game_st))]
	//>>* [OnValue (ifValue game_is_over (show_winner color game_st))]

determine_winner :: !GameSt -> Maybe (Color, String)
determine_winner {players}
  = case [player \\ player=:{ligretto} <- players | isEmpty ligretto] of
      [{color, name} : _] = Just (color, name)
      _                   = Nothing

game_is_over :: GameSt -> Bool
game_is_over gameSt
	= isJust (and_the_winner_is gameSt)

show_winner :: Color (Shared GameSt) GameSt -> Task (Color,String)
show_winner me game_st gameSt
	= accolades winner me game_st >>| return winner
where
	{color,name} = fromJust (and_the_winner_is gameSt)
	winner		 = (color,name)

game_over :: !Color !(Shared GameSt) !GameSt -> Maybe (Task (Color,String))
game_over me game_st gameSt
  =                    and_the_winner_is gameSt
  >>= \{color,name} -> (let winner = (color,name)
					   in return (accolades winner me game_st >>| return winner))
	//= case and_the_winner_is gameSt of
		//Just {color,name} = let winner = (color,name)
							 //in Just (accolades winner me game_st >>| return winner)
		//_                 = Nothing

accolades :: !(!Color,!String) !Color !(Shared GameSt) -> Task GameSt
accolades winner me game_st
	= viewSharedInformation ("The winner is " <+++ winner) [imageView (player_perspective me) (\_ _ -> Nothing)] game_st

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
              {onclick = const (play_row_card row_card.back no), local = False}
	\\ row_card <- row 
	 & no       <- [1..]
	]

hand_images :: !Bool !Hand !Color -> [Image GameSt]
hand_images interactive {conceal,discard} color
  #! conceal_pile = pile_image Back  conceal
  #! discard_pile = pile_image Front discard
  = [ tuneIf interactive conceal_pile {onclick = const (play_concealed_pile color), local = False}
    , tuneIf interactive discard_pile {onclick = const (play_hand_card color), local = False}
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
