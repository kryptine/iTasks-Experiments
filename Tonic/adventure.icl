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

:: Room 		=	{ name		 :: String
					, number 	 :: Int
					, status	 :: RoomStatus
					, inventory	 :: [Object]
					, exits		 :: [Exit]
					, present	 :: [Actor]
					}
:: RoomStatus 	=	Normal
				|	Alert [Detector]
:: Detector		=	Fire 
				| 	Smoke 
:: Object 		= 	FireExtinguisher
				| 	Blanket
				| 	Waterhose
:: Exit			=	North Int
				|	East Int
				|	South Int
				|	West Int
				|	Up Int
				|	Down Int
:: Actor		=	{ userName	:: User
					, carrying	:: [Object]
					, status	:: ActorStatus
					}
:: ActorStatus	= Available | NotAvailable | NormalTask | UrgentTask | VeryUrgentTask 
:: Floor		:==	[[Room]]
:: MAP			:== [Floor]

derive class iTask Room, RoomStatus, Detector, Object, Exit, Actor, ActorStatus

// small utility functions 

instance == Object where (==) o1 o2 = o1 === o2
instance == Actor  where (==) a1 a2 = a1.userName == a2.userName

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East i) = i
fromExit (South i) = i
fromExit (West i) = i
fromExit (Up i) = i
fromExit (Down i) = i

// utility functions on Room

leaving :: Actor Room -> Room
leaving actor room = {room & present = removeMember actor room.present}

entering :: Actor Room -> Room
entering actor room = {room & present = [actor:room.present]}

fetchObject :: Actor Object Room -> Room
fetchObject actor object room = {room & inventory = removeMember object room.inventory}

dropObject :: Actor Object Room -> Room
dropObject actor object room = {room & inventory = [object:room.inventory]}

updateActor :: Actor Room -> Room
updateActor actor room = {room & present = [actor:removeMember actor room.present]}

// tasks on Room

updateRoom :: Int (Room -> Room) -> Task ()
updateRoom roomNumber updRoom = upd (updateRoom` roomNumber updRoom) myMap >>| return ()
where 
	updateRoom` :: Int (Room -> Room) MAP -> MAP 
	updateRoom` i upd [] 	  			= []
	updateRoom` i upd [floor:floors]   	= [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
	where
		updateThisRoom room = if (i == room.number) (upd room)  room


findRoom :: Actor -> Task Room
findRoom actor 
	= 				get myMap 
		>>= \map -> case (myRoom actor map) of
						[] 		-> 		askPosition actor.userName  					// not administrated yet, add it
									>>| findRoom actor
						rooms   -> 		return (hd rooms)

myRoom :: Actor MAP -> [Room]
myRoom actor map = 	[ room 
					\\ floor <- map, layer <- floor, room <- layer, {userName} <- room.present 
					| actor.userName == userName
					] 

findAllActors :: MAP ->  [(Int,Actor)]
findAllActors map =	[ (room.number,actor)
					\\ floor <- map, layer <- floor, room <- layer, actor <- room.present 
					]

// defining a map as example

myMap  :: Shared MAP
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



			
// tasks

showMap = 		viewSharedInformation "map status:" [] myMap 

askPosition :: User  -> Task Int
askPosition user 
	=							enterInformation "You are lost! What is your location? " [] 
		>>= \location ->		updateRoom location (entering {userName = user, carrying = [], status = defaultValue})
		>>|						return location

:: Instruction  = Goto 		Int 
				| Fetch 	Object
				| Drop 		Object
				| Fight  	Object
				| Resque	Object
				| Solve 	String

derive class iTask Instruction

followInstructions :: Actor [Instruction] -> Task ()
followInstructions _ [] 		= return ()
followInstructions actor todo 	= doInstructions actor todo >>| return ()

doInstructions :: Actor [Instruction] -> Task Actor
doInstructions actor [] = return actor
doInstructions actor todo=:[instruction:rest]
	= 					findRoom actor
		>>= \room ->	viewSharedInformation "my room" [ViewWith (myRoom actor)] myMap
		>>*				[ OnAction (Action ("Take Exit " <+++ exit) []) (always (move actor room.number (fromExit exit)))
						\\ exit <- room.exits
						]
						++ 
						[ OnAction (Action ("Fetch " <+++ object) [])  	(always (pickupObject actor room object))
						\\ object <- room.inventory
						]
						++
						[ OnAction (Action ("Drop " <+++ object) [])  	(always (dropDownObject actor room object))
						\\ object <- actor.carrying
						]
		>>= \actor ->	doInstructions actor todo

pickupObject actor room object
	=				updateRoom room.number (fetchObject actor object)
	>>|				return {actor & carrying = [object:actor.carrying]}
	>>= \actor ->	updateRoom room.number (updateActor actor)
	>>| 			return actor

dropDownObject actor room object
	=				updateRoom room.number (dropObject actor object)
	>>|				return {actor & carrying = removeMember object actor.carrying}
	>>= \actor ->	updateRoom room.number (updateActor actor) 
	>>| 			return actor

move :: Actor Int Int -> Task Actor
move actor fromRoom toRoom
	= 				
	  			 	updateRoom fromRoom (leaving actor)
	  	>>| 		updateRoom toRoom (entering actor)
		>>|			return actor

showInstruction :: Room [Instruction] -> Task ()
showInstruction room todo
	=	(viewInformation "You are here:" [] room
		-&&-
		viewInformation "Your current instructions are:" [ViewWith (\i -> toSingleLineText i <+++ ";")] todo)
		>>| 
		return ()

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
	 									-> 			updateRoom roomNumber (updateActor {actor & Actor.status = status})
	 										>>|		appendTopLevelTaskFor actor.userName False (followInstructions actor todo) 
	 										>>| 	return ()
	 											)
	 								 )
            , OnAction  ActionCancel (always (return ()))
            ]
	 )


						




