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
					}
:: Floor		:==	[[Room]]
:: MAP			:== [Floor]

derive class iTask Room, RoomStatus, Detector, Object, Exit, Actor

// utility functions 

instance == Object where (==) o1 o2 = o1 === o2
instance == Actor  where (==) a1 a2 = a1.userName == a2.userName

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East i) = i
fromExit (South i) = i
fromExit (West i) = i
fromExit (Up i) = i
fromExit (Down i) = i

updMap :: Int (Room -> Room) MAP -> MAP 
updMap i upd [] 	  			= []
updMap i upd [floor:floors]   	= [[map updateRoom rooms \\ rooms <- floor]: updMap i upd floors]
where
	updateRoom room = if (i == room.number) (upd room)  room

leaving :: Actor Room -> Room
leaving actor room = {room & present = removeMember actor room.present}

entering :: Actor Room -> Room
entering actor room = {room & present = [actor:room.present]}

fetchObject :: Actor Object Room -> Room
fetchObject actor object room = {room & inventory = removeMember object room.inventory}

dropObject :: Actor Object Room -> Room
dropObject actor object room = {room & inventory = [object:room.inventory]}

adjustActor :: Actor Room -> Room
adjustActor actor room = {room & present = [actor:removeMember actor room.present]}

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

// setting up a simple resource 

:: Resource =	{ acting	:: Actor
				, status	:: ResourceStatus
				}
:: ResourceStatus	= Available | NotAvailable | NormalTask | UrgentTask | VeryUrgentTask 

instance == Resource
where
	(==) r1 r2 = r1.Resource.acting.userName == r2.Resource.acting.userName 

myResources :: Shared [Resource]
myResources = sharedStore "myResource" []

updateResources :: Actor ResourceStatus -> Task Actor 
updateResources actor status 
	= upd updateResource myResources >>| return actor
where
	updateResource :: [Resource] -> [Resource]
	updateResource resources
	 =  if (isMember thisResource resources) (updatedResources resources) [thisResource:resources]
	 
	updatedResources :: [Resource] -> [Resource]
	updatedResources resources 
	 = [if (resource.acting.userName == actor.userName) thisResource resource \\ resource <- resources] // update administration

	thisResource :: Resource
	thisResource = {acting = actor, status = status}

derive class iTask Resource, ResourceStatus
			
// tasks

showMap = 		viewSharedInformation "map status:" [] myMap 
				-&&-
				viewSharedInformation "resource status:" [] myResources

updateMap :: Int (Room -> Room) -> Task ()
updateMap roomNumber updRoom = upd (updMap roomNumber updRoom) myMap >>| return ()
 
getRoom :: User -> Task Room
getRoom user 
	= 				get myMap 
		>>= \map -> case (myRoom user map) of
						[] 		-> 		askPosition user  					// not administrated yet, add it
									>>| getRoom user
						rooms   -> 		return (hd rooms)

myRoom user map = 	[ room 
					\\ floor <- map, layer <- floor, room <- layer, {userName} <- room.present 
					| user == userName
					] 

askPosition :: User  -> Task Int
askPosition user 
	=							enterInformation "in which room number are you currently located?" [] 
		>>= \location ->		enterChoice "what is your status?" [] [Available,NotAvailable, NormalTask,UrgentTask,VeryUrgentTask]
		>>= \availability ->	updateMap location (entering {userName = user, carrying = []})
		>>|						updateResources {userName = user, carrying = []} availability
		>>|						return location

:: Instruction  = Goto 		Int 
				| Fetch 	Object
				| Drop 		Object
				| Fight  	Object
				| Resque	Object
				| Solve 	String

derive class iTask Instruction

followInstructions :: User ResourceStatus [Instruction] -> Task ()
followInstructions me status [] = return ()
followInstructions me status todo 
	=				updateResources {userName = me, carrying = []} status
	 >>= \actor ->	doInstructions actor todo
	 >>= \actor ->	updateResources actor Available
	 >>|			return ()


doInstructions :: Actor [Instruction] -> Task Actor
doInstructions actor [] = return actor
doInstructions actor todo=:[instruction:rest]
	= 					getRoom actor.userName
		>>= \room ->	viewSharedInformation "my room" [ViewWith (myRoom actor.userName)] myMap
		>>*				[ OnAction (Action ("Take Exit " <+++ exit) []) (always (move actor room.number (fromExit exit)))
						\\ exit <- room.exits
						]
						++ 
						[ OnAction (Action ("Fetch " <+++ x) [])  	(always (		updateMap room.number (fetchObject actor x)
																				>>| let nactor = {actor & carrying = [x:actor.carrying]} in
																						(updateMap room.number (adjustActor nactor) >>| return nactor)
																			) ) 
						\\ x <- room.inventory
						]
						++
						[ OnAction (Action ("Drop " <+++ x) [])  	(always (		updateMap room.number (dropObject actor x)
																				>>| let nactor = {actor & carrying = removeMember x actor.carrying} in
																					   (updateMap room.number (adjustActor nactor) >>| return nactor)
																			 ) )
						\\ x <- actor.carrying
						]
		>>= \actor ->	doInstructions actor todo

/*
doInstructions :: User [Instruction] -> Task ()
doInstructions me [] = return ()
doInstructions me todo=:[instruction:rest]
	= 					viewSharedInformation "my room" [ViewWith (myRoom me)] myMap
		>>|				getRoom me
		>>= \room ->	case instruction of 
							(Goto i) -> if (room.number == i) 			(doInstructions me rest)
															  			(move me todo room)
							(Fetch x) -> if (isMember x room.inventory) (		updateMap room.number (fetchObject me x)
																		 >>|	doInstructions me rest
																		 )
															  			(move me todo room)
							(Drop x) ->  (		updateMap room.number (dropObject me x)
											>>|	doInstructions me rest
										 )
							_		 -> return ()

*/

move :: Actor Int Int -> Task Actor
move actor fromRoom toRoom
	= 				
	  			 	updateMap fromRoom (leaving actor)
	  	>>| 		updateMap toRoom (entering actor)
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
	  (		(	(enterInformation "Define Instructions" [] 
				-&&-
				enterChoiceWithShared "Assign worker" [] myResources)
				-&&-
				enterChoice "Assign Urgency" [] [NormalTask,UrgentTask,VeryUrgentTask] 
			)
	 >>* 	[ OnAction  ActionOk     (hasValue (\((todo,resource),urgency) -> appendTopLevelTaskFor resource.acting.userName False (followInstructions resource.acting.userName urgency todo) >>| return ()))
            , OnAction  ActionCancel (always (return ()))
            ]
	 )


						




