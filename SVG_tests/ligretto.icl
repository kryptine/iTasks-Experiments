module ligretto

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin
import StdArray

Start :: *World -> *World
Start world = startTask [ workflow "ligretto"      "ligretto"          	      play_ligretto
						, workflow "Manage users"  "Manage system users..."   manageUsers
						] world


startTask taskList world
	= startEngine [ publish "/" (WebApp []) (\_-> browseExamples taskList)
				  ] world
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


// ligretto 

import ligrettoModel

derive class iTask Player, Color, Hand, Card

play_ligretto :: Task Color
play_ligretto
	=               get currentUser
	>>= \me      -> invite_friends 
	>>= \friends -> set (repeatn 16 []) middle_state
	>>|             let nr_of_players = length friends + 1 in
	                anyTask
	                 [  player @: game nr_of_players color
	                 \\ player <- [me:friends] & color <- colors nr_of_players
	                 ]
	>>= \winner  -> allTasks
	                 [  player @: (viewInformation "The winner is:" [] winner >>= return)
	                 \\ player <- [me:friends]
	                 ]
	>>| return winner


invite_friends :: Task [User]
invite_friends
	=               enterSharedMultipleChoice "Select friends to play with" [] users
	>>= \friends -> if (not (isMember (length friends) [1..3]))
	                   (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	                   (return friends)


game :: Int Color -> Task Color
game nr_of_players color
	=           get randomInt
	  >>= \r -> let player = initial_player nr_of_players color (abs r) in
	            set player (player_state color)
	  >>|       viewSharedInformation "Middle" [ViewWith piles_view] middle_state
	  ||-       viewSharedInformation ("Player " <+++ color) [ViewWith player_view] (player_state color)
	  ||-       play_cards nr_of_players player

play_cards nr_of_players player
	= watch (player_state player.color >+< middle_state)
	>>* (  [  OnAction (Action ("Play card "<+++cardnr) []) (play_card nr_of_players player cardnr) 
	       \\ cardnr <- [1..nr_of_cards_in_row nr_of_players]
	       ]
	    ++ [  OnAction (Action "Play hand"    []) (play_hand nr_of_players player)
	       ,  OnAction (Action "Next hand"    []) (next_hand nr_of_players player)
	       ,  OnAction (Action "Shuffle hand" []) (shuffle   nr_of_players player)
	       ,  OnValue  (player_wins)
	       ]
	    )

player_wins :: (TaskValue (Player,Middle)) -> Maybe (Task Color)
player_wins (Value (me,middle) _)
| isEmpty me.ligretto	= Just (return me.color)
| otherwise				= Nothing
player_wins _			= Nothing

shuffle :: Int Player (TaskValue (Player,Middle)) -> Maybe (Task Color)
shuffle nr_of_players player (Value (me,middle) _)
| not (isEmpty conceal)	= Nothing
| otherwise				= Just (    get randomInt
						  >>= \r -> set (shuffle_hand (abs r) me) (player_state player.color)
						  >>|       play_cards nr_of_players player
						  )
where
	conceal				= me.hand.conceal
shuffle _ _ _			= Nothing

next_hand :: Int Player (TaskValue (Player,Middle)) -> Maybe (Task Color)
next_hand nr_of_players player (Value (me,middle) _)
| isEmpty conceal		= Nothing
| otherwise				= Just (set (swap_discards me) (player_state player.color)
						    >>| play_cards nr_of_players player
						  )
where
	conceal				= me.hand.conceal
next_hand _ _ _			= Nothing

play_card :: Int Player Int (TaskValue (Player,Middle)) -> Maybe (Task Color)
play_card nr_of_players player cardnr (Value (me,middle) _)
| cardnr > length me.row= Nothing
| isEmpty matching_piles= Nothing
| otherwise				= let (pilenr,pile) = hd matching_piles in
						  Just (update (updateAt pilenr [card : pile]) middle_state 
						    >>| set (move_ligretto_card_to_row cardnr me) (player_state player.color)
						    >>| play_cards nr_of_players player
						  )
where
	card				= row_card cardnr me
	matching_piles		= [(pilenr,pile) \\ pile <- middle & pilenr <- [0..] | card_matches_top_of_pile card pile]
play_card _ _ _ _		= Nothing

play_hand :: Int Player (TaskValue (Player,Middle)) -> Maybe (Task Color)
play_hand nr_of_players player (Value (me,middle) _)
| isNothing maybe_card	= Nothing
| isEmpty matching_piles= Nothing
| otherwise				= let (pilenr,pile) = hd matching_piles in
						  Just (update (updateAt pilenr [card : pile]) middle_state
						    >>| set (remove_top_of_discard me) (player_state player.color)
						    >>| play_cards nr_of_players player
						  )
where
	maybe_card			= top_discard me
	card				= fromJust maybe_card
	matching_piles		= [(pilenr,pile) \\ pile <- middle & pilenr <- [0..] | card_matches_top_of_pile card pile]
play_hand _ _ _			= Nothing


view_player :: !Player -> Task Player
view_player player
	= viewInformation ("Player " <+++ player.color) [ViewWith player_view] player

player_view :: !Player -> HtmlTag
player_view player
	= TableTag [BorderAttr "2"] 
	           [view_row      player.row
	           ,view_ligretto player.ligretto
	           ,view_hand     player.hand
	           ]

view_row :: !RowPlayer -> HtmlTag
view_row row
	= TableTag []
	           [TrTag [] [TdTag [] [Text (if (i==1) "row" "")] \\ i <- [1..length row]]
	           ,TrTag [] [TdTag [] [Text (toString i)]         \\ i <- [1..length row]]
	           ,TrTag [] [view_card Front card                 \\ card <- row]
	           ]

view_card :: !SideUp !Card -> HtmlTag
view_card side_up card
	= TdTag [StyleAttr (glue_style_atts [("text-align","center")
	                                    ,("background",""<+++color)
	                                    ,("padding","20px")
	                                    ,("color",if (color===Yellow) "black" "white")
	                                    ])]
	        [Text content]
where
	(color,content)	= case side_up of
						Front = (card.front, toString card.nr)
						Back  = (card.back,  "-")

view_ligretto :: !Pile -> HtmlTag
view_ligretto pile
	= TableTag []
	           [TrTag [] [TdTag [] [Text "ligretto"]]
	           ,if (isEmpty pile) 
	               (TrTag [] [TdTag [] [Text "empty!"]])
	               (TrTag [] [view_card Front (hd pile)])
	           ]

view_hand :: !Hand -> HtmlTag
view_hand {conceal,discard}
	= TableTag []
	           [TrTag [] [TdTag [] [Text "hand"]]
	           ,TrTag [] [if (isEmpty conceal) (TdTag [] [Text "empty!"]) (view_card Back  (hd conceal))
	                     ,if (isEmpty discard) (TdTag [] [Text "empty!"]) (view_card Front (hd discard))
	                     ]
	           ]

view_piles :: ![Pile] -> Task [Pile]
view_piles piles
	= viewInformation "Piles" [ViewWith piles_view] piles

piles_view :: ![Pile] -> HtmlTag
piles_view piles
	= TableTag [BorderAttr "2"]
	           [TrTag [] [  if (isEmpty pile) (TdTag [] [Text "empty!"])
	                                          (view_card Front (hd pile))
	                     \\ pile <- piles
	                     ]
	           ,TrTag [] [TdTag [] [Text (toString i)] \\ i <- [1..length piles]]
	           ]

glue_style_atts atts = foldr (\(a,b) c -> a<+++":"<+++b<+++";"<+++c) "" atts

middle_state :: Shared Middle
middle_state = sharedStore "middle" (repeatn 16 [])

player_state :: Color -> Shared Player
player_state color = sharedStore ("player " <+++ color) {color=color,row=[],ligretto=[],hand={conceal=[],discard=[]}}