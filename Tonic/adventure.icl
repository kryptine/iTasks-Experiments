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
					, present	 :: [User]
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
:: Floor		:==	[[Room]]
:: MAP			:== [Floor]

derive class iTask Room, RoomStatus, Detector, Object, Exit

// utility functions 

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

leaving :: User Room -> Room
leaving user room = {room & present = removeMember user room.present}

entering :: User Room -> Room
entering user room = {room & present = [user:room.present]}

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

:: Resource =	{ userName	:: User
				, status	:: ResourceStatus
				}
:: ResourceStatus	= Available | NotAvailable | NormalTask | UrgentTask | VeryUrgentTask 

instance == Resource
where
	(==) r1 r2 = r1.Resource.userName == r2.Resource.userName 

myResources :: Shared [Resource]
myResources = sharedStore "myResource" []

updateResources :: User ResourceStatus -> Task [Resource] 
updateResources user status 
	= upd updateResource myResources
where
	updateResource :: [Resource] -> [Resource]
	updateResource resources
	 =  if (isMember thisResource resources) (updatedResources resources) [thisResource:resources]
	 
	updatedResources :: [Resource] -> [Resource]
	updatedResources resources 
	 = [if (resource.userName == user) thisResource resource \\ resource <- resources] // update administration

	thisResource :: Resource
	thisResource = {userName = user, status = status}

derive class iTask Resource, ResourceStatus
			
// tasks

showMap = 		viewSharedInformation "map status:" [] myMap 
				-&&-
				viewSharedInformation "resource status:" [] myResources

updateMap :: Int (Room -> Room) -> Task MAP
updateMap roomNumber updRoom = upd (updMap roomNumber updRoom) myMap

getRoom :: User -> Task Room
getRoom user 
	= 				get myMap 
		>>= \map -> case (myRoom map) of
						[] 		-> 		askPosition user  					// not administrated yet, add it
									>>| getRoom user
						rooms   -> 		return (hd rooms)
where
	myRoom map = [room \\ floor <- map, layer <- floor, room <- layer | isMember user room.present] 

askPosition :: User  -> Task Int
askPosition user 
	=							enterInformation "in which room number are you currently located?" [] 
		>>= \location ->		enterChoice "what is your status?" [] [Available,NotAvailable, NormalTask,UrgentTask,VeryUrgentTask]
		>>= \availability ->	updateMap location (entering user)
		>>|						updateResources user availability
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
	=					updateResources me status
	 >>| 				doInstructions me todo


doInstructions :: User [Instruction] -> Task ()
doInstructions me [] = return ()
doInstructions me todo=:[instruction:rest]
	= 					getRoom me
		>>= \room ->	case instruction of 
							(Goto i) -> if (room.number == i) (doInstructions me rest)
															  (				(showInstruction room todo ||-
															  				enterChoice "choose exit" [] room.exits)
															  	>>= \i -> 	updateMap room.number (leaving me)
															  	>>| 		updateMap (fromExit i) (entering me)
															  	>>|			doInstructions me todo 
															  	)
							_		 -> return ()
							

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
	 >>* 	[ OnAction  ActionOk     (hasValue (\((todo,actor),urgency) -> appendTopLevelTaskFor actor.userName False (followInstructions actor.userName urgency todo) >>| return ()))
            , OnAction  ActionCancel (always (return ()))
            ]
	 )


						




