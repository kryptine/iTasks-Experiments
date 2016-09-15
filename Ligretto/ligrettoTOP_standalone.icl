implementation module ligrettoTOP_standalone

import ligrettoTOP
import iTasks

/** Running the Ligretto game as a stand-alone iTask server application:
*/
:: Friend = { friend   :: String
            , password :: Password
            }
:: OwnerChoice = UpdateFriends | PlayLigretto

derive class iTask Friend, OwnerChoice

/*  Register the persons with whom you wish to play Ligretto.
    This is shared as a list of (friend,color) pairs (yup, that is quite unsafe...).
    In this simplified version, there are two requirements that need to be adhered to:
    (1) The list is not empty, and the first entry is the 'owner' of the game.
        Only the 'owner' can update this list.
    (2) Each color occurs at most once (so in this version there is a maximum of four players).
*/
registered_players :: Shared [(Friend,Color)]
registered_players
	= sharedStore "my_ligretto_friends" 
          [({friend = "owner", password = Password "owner"},  Red)
          ,({friend = "blue",  password = Password "blue"},   Blue)
          ,({friend = "yellow",password = Password "yellow"}, Yellow)
          ,({friend = "green", password = Password "green"},  Green)
          ]

Start :: *World -> *World
Start world = startEngine (get registered_players >>= login) world //check_registered_players world
where
	check_registered_players
		=              get registered_players
		>>= \people -> let nr_of_persons = length people in
		               if (nr_of_persons < 1 || nr_of_persons > 4) 
		                  (ouch "Incorrect number of registered persons." >>| return Void)	
		                  (login people)
	login people 
		= forever (
		  (   viewTitle "Enter Ligretto"
		  ||- enterInformation ("Login","Enter your credentials to login") [])
		  >>* [OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (check_login people))]
		  )
	
	check_login [(owner,color):friends] person
		=  if (person === owner) 
		      (update_friends_or_play_ligretto (length friends + 1) color)
              (case [color \\ (friend,color) <- friends | person === friend] of
                    [color : _] -> play_ligretto_as (length friends + 1) color >>| return Void
                    nobody      -> ouch "Your username or password is incorrect" >>| return Void
              )
	
	update_friends_or_play_ligretto nr_of_players color
		=              enterChoice "What do you want to do?" [] [PlayLigretto,UpdateFriends]
		>>= \choice -> case choice of
		                   PlayLigretto  -> play_ligretto_as nr_of_players color >>| return Void
		                   UpdateFriends -> updateSharedInformation "Update friends:" [] registered_players >>| check_registered_players

/** ouch message = task:
        this task always displays message.
*/
ouch :: String -> Task String
ouch message = viewInformation (Title "Failure") [] message
