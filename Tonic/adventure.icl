module adventure
 
import StdArray
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

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

:: Room roomstatus object actorstatus
				=	{ name		 :: String
					, number 	 :: Int
					, status	 :: roomstatus
					, inventory	 :: [object]
					, exits		 :: [Exit]
					, present	 :: [Actor object actorstatus]
					}
:: Exit			=	North Int
				|	East Int
				|	South Int
				|	West Int
				|	Up Int
				|	Down Int
:: Actor o a
				=	{ userName	:: User
					, carrying	:: [o]
					, status	:: a
					}
:: Floor s o a	:==	[[Room s o a]]
:: MAP s o a	:== [Floor s o a]

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

leaving :: (Actor o a) (Room s o a) -> (Room s o a) | Eq o
leaving actor room = {room & present = removeMember actor room.present}

entering :: (Actor o a) (Room s o a) -> (Room s o a)
entering actor room = {room & present = [actor:room.present]}

fetchObject :: o (Room s o a) -> (Room s o a) | Eq o
fetchObject object room = {room & inventory = removeMember object room.inventory}

dropObject ::  o (Room s o a) -> (Room s o a)
dropObject object room = {room & inventory = [object:room.inventory]}

updateActor :: (Actor o a) (Room s o a) -> (Room s o a) | Eq o
updateActor actor room = {room & present = [actor:removeMember actor room.present]}

// tasks on Room

doInstructions :: (Actor o a) (Shared (MAP s o a)) -> Task (Actor o a) | iTask s & iTask o & iTask a & Eq o
doInstructions actor smap
	= 	(whileUnchanged smap
		(\map ->		let room = hd (myRoom actor map) in
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
		>>= \actor ->	doInstructions actor smap
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

updateRoom :: Int ((Room s o a)-> (Room s o a)) (Shared (MAP s o a))-> Task () | iTask s & iTask o & iTask a
updateRoom roomNumber updRoom smap 
	= 	upd (updateRoom` roomNumber updRoom) smap 
	>>| return ()
where 
	updateRoom` i upd [] 	  			= []
	updateRoom` i upd [floor:floors]   	= [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
	where
		updateThisRoom room = if (i == room.number) (upd room)  room


findRoom :: (Actor o a) (Shared (MAP s o a)) -> Task (Room s o a)| iTask s & iTask o & iTask a
findRoom actor smap
	= 				get smap 
		>>= \map -> case (myRoom actor map) of
						rooms   -> 		return (hd rooms)

/*

findRoom :: (Actor o a) (Shared (MAP s o a)) -> Task (Room s o a)| iTask s & iTask o & iTask a
findRoom actor smap
	= 				get smap 
		>>= \map -> case (myRoom actor map) of
						[] 		-> 		askPosition actor.userName  					// not administrated yet, add it
									>>| findRoom actor
						rooms   -> 		return (hd rooms)

*/

move :: (Actor o a) Int Int (Shared (MAP s o a))-> Task (Actor o a)| iTask s & iTask o & iTask a & Eq o
move actor fromRoom toRoom smap
	= 				
	  			 	updateRoom fromRoom (leaving actor) smap
	  	>>| 		updateRoom toRoom (entering actor) smap
		>>|			return actor


myRoom :: (Actor o a) (MAP s o a) -> [(Room s o a)]
myRoom actor map = 	[ room 
					\\ floor <- map, layer <- floor, room <- layer, {userName} <- room.present 
					| actor.userName == userName
					] 

findAllActors :: (MAP s o a) ->  [(Int,(Actor o a))]
findAllActors map =	[ (room.number,actor)
					\\ floor <- map, layer <- floor, room <- layer, actor <- room.present 
					]

// defining a concrete map as example

:: MyMap		:== MAP RoomStatus Object ActorStatus
:: MyActor		:== Actor Object ActorStatus
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
	room1		= {name = "room 1",   number = 1, status = Normal, inventory = [], exits = [South 4], present = []}			
	room2		= {name = "room 2",   number = 2, status = Normal, inventory = [], exits = [South 4], present = []}			
	room3		= {name = "room 3",   number = 3, status = Normal, inventory = [FireExtinguisher], exits = [South 4], present = []}
	corridor	= {name = "corridor", number = 4, status = Normal, inventory = [], exits = [North 1, North 2, North 3
																						   ,South 5, South 6, South 7
																						   ], present = []}
	room4		= {name = "room 4",   number = 5, status = Normal, inventory = [], exits = [North 4], present = []}			
	room5		= {name = "room 5",   number = 6, status = Normal, inventory = [], exits = [North 4], present = []}			
	room6		= {name = "room 6",   number = 7, status = Normal, inventory = [FireExtinguisher], exits = [North 4], present = []}
			
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
followInstructions actor todo 	= doInstructions actor myMap >>| return ()

/*

showInstruction :: (Room s o a) [Instruction] -> Task ()
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


						




