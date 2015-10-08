module adventure
 
import StdArray
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

import StdMisc

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "Adventure" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
		, publish "/map"   (WebApp []) (\_-> showMap)
        ] world
 
myExamples :: [Workflow]
myExamples = 	[	workflow "give instructions"			"give instructions to a worker" 		giveInstructions			
				,	workflow "administrate as present"		"tell where you are currently located"  (get currentUser >>= \me -> askPosition me)
			 	]

:: Room roomStatus object actorStatus
				=	{ name		 :: String
					, number 	 :: Int
					, exits		 :: [Exit]
					, status	 :: roomStatus
					, inventory	 :: [object]
					, actors	 :: [Actor object actorStatus]
					}
:: Exit			=	North Int
				|	East Int
				|	South Int
				|	West Int
				|	Up Int
				|	Down Int
:: Actor o a	=	{ userName	:: User
					, carrying	:: [o]
					, status	:: a
					}
:: Floor r o a	:==	[[Room r o a]]
:: MAP r o a	:== [Floor r o a]

derive class iTask Room, Exit, Actor

// small utility functions 

instance == (Actor o a)  where (==) a1 a2 = a1.userName == a2.userName

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East i) = i
fromExit (South i) = i
fromExit (West i) = i
fromExit (Up i) = i
fromExit (Down i) = i

// utility functions on Room

leaving :: (Actor o a) (Room r o a) -> (Room r o a) | Eq o
leaving actor room = {room & actors = removeMember actor room.actors}

entering :: (Actor o a) (Room r o a) -> (Room r o a)
entering actor room = {room & actors = [actor:room.actors]}

fetchObject :: o (Room r o a) -> (Room r o a) | Eq o
fetchObject object room = {room & inventory = removeMember object room.inventory}

dropObject ::  o (Room r o a) -> (Room r o a)
dropObject object room = {room & inventory = [object:room.inventory]}

updateActor :: (Actor o a) (Room r o a) -> (Room r o a) | Eq o
updateActor actor room = {room & actors = [actor:removeMember actor room.actors]}

myRoom :: (Actor o a) (MAP r o a) -> (Room r o a)
myRoom actor map 
# rooms	=	[ room 
			\\ floor <- map, layer <- floor, room <- layer, {userName} <- room.actors 
			| actor.userName == userName
			] 
= case rooms of 
	[]  -> abort "cannot find room of actor"
	_	-> hd rooms

findAllActors :: (MAP r o a) ->  [(Int,(Actor o a))]
findAllActors map =	[ (room.number,actor)
					\\ floor <- map, layer <- floor, room <- layer, actor <- room.actors 
					]

// moving around in the map

:: UserActions r o a :== [TaskCont (Room r o a) (Task (Actor o a))]

moveAround :: (Actor o a) (UserActions r o a) (Shared (MAP r o a)) -> Task (Actor o a) | iTask r & iTask o & iTask a & Eq o
moveAround actor actions smap
	= 	(whileUnchanged smap
		(\map ->		let room = myRoom actor map in
										viewInformation "my room" [] room
						>>*				[ OnAction (Action ("Take Exit " <+++ exit) []) (always (move actor room.number (fromExit exit) smap))
										\\ exit <- room.exits
										]
										++ 
										[ OnAction (Action ("Fetch " <+++ object) [])  	(always (pickupObject actor room object smap))
										\\ object <- room.inventory
										]
										++
										[ OnAction (Action ("Drop " <+++ object) [])  	(always (dropDownObject actor room object smap))
										\\ object <- actor.carrying
										]
		))
		>>= \actor ->	moveAround actor actions smap
where
	pickupObject actor room object smap
		=				updateRoom room.number (fetchObject object) smap
		>>|				return {actor & carrying = [object:actor.carrying]}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>| 			return actor
	
	dropDownObject actor room object smap
		=				updateRoom room.number (dropObject object) smap
		>>|				return {actor & carrying = removeMember object actor.carrying}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>| 			return actor

updateRoom :: Int ((Room r o a)-> (Room r o a)) (Shared (MAP r o a))-> Task () | iTask r & iTask o & iTask a
updateRoom roomNumber updRoom smap 
	= 	upd (updateRoom` roomNumber updRoom) smap 
	>>| return ()
where 
	updateRoom` i upd [] 	  			= []
	updateRoom` i upd [floor:floors]   	= [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
	where
		updateThisRoom room = if (i == room.number) (upd room)  room

move :: (Actor o a) Int Int (Shared (MAP r o a))-> Task (Actor o a)| iTask r & iTask o & iTask a & Eq o
move actor fromRoom toRoom smap
	= 				
	  			 	updateRoom fromRoom (leaving actor) smap
	  	>>| 		updateRoom toRoom (entering actor) smap
		>>|			return actor



// defining a concrete map as example

:: MyMap		:== MAP RoomStatus Object ActorStatus
:: MyActor		:== Actor Object ActorStatus
:: MyRoom		:== Room RoomStatus Object ActorStatus
:: RoomStatus 	=	Normal
				|	Alert [Detector]
:: Detector		=	Fire 
				| 	Smoke 
:: Object 		= 	FireExtinguisher
				| 	Blanket
				| 	Waterhose
:: ActorStatus	= Available | NotAvailable | NormalTask | UrgentTask | VeryUrgentTask 

instance == Object where (==) o1 o2 = o1 === o2


derive class iTask RoomStatus, Detector, Object, ActorStatus

myMap  :: Shared MyMap
myMap = sharedStore "myBuilding" [floor0]
where
	floor0  	= [[room1,room2,room3],[corridor],[room4,room5,room6]]
	room1		= {name = "room 1",   number = 1, status = Normal, inventory = [], exits = [South 4], actors = []}			
	room2		= {name = "room 2",   number = 2, status = Normal, inventory = [], exits = [South 4], actors = []}			
	room3		= {name = "room 3",   number = 3, status = Normal, inventory = [FireExtinguisher], exits = [South 4], actors = []}
	corridor	= {name = "corridor", number = 4, status = Normal, inventory = [], exits = [North 1, North 2, North 3
																						   ,South 5, South 6, South 7
																						   ], actors = []}
	room4		= {name = "room 4",   number = 5, status = Normal, inventory = [], exits = [North 4], actors = []}			
	room5		= {name = "room 5",   number = 6, status = Normal, inventory = [], exits = [North 4], actors = []}			
	room6		= {name = "room 6",   number = 7, status = Normal, inventory = [FireExtinguisher], exits = [North 4], actors = []}
			
// tasks on this specific MAP type

showMap = 		viewSharedInformation "map status:" [] myMap 

askPosition :: User  -> Task Int
askPosition user 
	=							enterInformation "You are lost! What is your location? " [] 
		>>= \location ->		updateRoom location (entering {userName = user, carrying = [], status = defaultValue}) myMap
		>>|						return location

:: Instruction  = Goto 		Int 
				| Fetch 	Object
				| Drop 		Object
				| Fight  	Object
				| Resque	Object
				| Solve 	String

derive class iTask Instruction

followInstructions :: MyActor [Instruction] -> Task ()
//followInstructions _ [] 		= return ()
followInstructions actor todo 	= moveAround actor [] myMap >>| return ()

/*

showInstruction :: (Room r o a) [Instruction] -> Task ()
showInstruction room todo
	=	(viewInformation "You are here:" [] room
		-&&-
		viewInformation "Your current instructions are:" [ViewWith (\i -> toSingleLineText i <+++ ";")] todo)
		>>| 
		return ()
*/

// define a set of instructions to be done and someone to work on it 

giveInstructions :: Task ()
giveInstructions 
	= forever
	  (				get myMap
	   >>= \map -> (	(enterInformation "Define Instructions" [] 
						-&&-
						enterChoice "Assign worker" [] (findAllActors map))
						-&&-
						enterChoice "Assign Status" [] [NormalTask,UrgentTask,VeryUrgentTask] 
					)
	  >>* 	[ OnAction  ActionOk     (hasValue (\((todo,(roomNumber,actor)),status) 
	 									-> 			updateRoom roomNumber (updateActor {actor & Actor.status = status}) myMap
	 										>>|		appendTopLevelTaskFor actor.userName False (followInstructions actor todo) 
	 										>>| 	return ()
	 											)
	 								 )
            , OnAction  ActionCancel (always (return ()))
            ]
	 )

myActions :: [TaskCont MyRoom MyActor]
myActions = undef
						

f :: (Room r o a) (Actor o a) -> (UserActions r o a) |  iTask r & iTask o & iTask a & Eq o
f room actor = [OnAction (Action "Id" []) (withValue (\_ ->  Just (return actor)))]


