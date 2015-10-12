module shipAdventure
 
import adventure
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import Graphics.Scalable

import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap

:: MyMap		:== MAP RoomStatus Object ActorStatus
:: MyActor		:== Actor Object ActorStatus
:: MyRoom		:== Room RoomStatus Object ActorStatus

:: RoomStatus 	:==	[Detector] 
:: Detector		=	FireDetector Bool 
				| 	SmokeDetector Bool
:: Object 		= 	FireExtinguisher
				| 	Blanket
:: ActorStatus	= 	{ occupied	:: Availability
					, todo		:: [(Instruction,Priority)]
					}
:: Availability	=	Available | NotAvailable | Busy  

:: Instruction	=	FightFireInRoom Int | InspectSmokeInRoom Int | GotoRoom Int 
:: Priority		=	NormalPriority | HighPriority | Urgent | Vital

derive class iTask Detector, Object, ActorStatus, Availability, Instruction, Priority

instance == Object 		where (==) o1 o2 = o1 === o2
instance == Instruction where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2

isHigh (FireDetector  b) = b 
isHigh (SmokeDetector b) = b


floorImage = rect (px 100.0) (px 100.0)

roomImage {exits}
  #! (northEs, eastEs, southEs, westEs, upEs, downEs) = foldr foldExit ([], [], [], [], []) exits
  = floorImage
  where
  foldExit (North n) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([n : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit (East n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [n : eastEs], southEs, westEs, upEs, downEs)
  foldExit (South n) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [n : southEs], westEs, upEs, downEs)
  foldExit (West n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [n : westEs], upEs, downEs)
  foldExit (Up n)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [n : upEs], downEs)
  foldExit (Down n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [n : downEs])

myMap  :: Shared MyMap
myMap = sharedStore "myBuilding" [floor0]
where
	floor0  	= 'DIS'.fromList [ (1, room1), (2, room2), (3, room3)
                                 , (4, corridor)
                                 , (5, room4), (6, room5), (7, room6)]
	room1		= {name = "room 1",   number = 1, roomStatus = detectors, inventory = [], exits = [South 4], actors = []}			
	room2		= {name = "room 2",   number = 2, roomStatus = detectors, inventory = [], exits = [South 4], actors = []}			
	room3		= {name = "room 3",   number = 3, roomStatus = detectors, inventory = [FireExtinguisher], exits = [South 4], actors = []}
	corridor	= {name = "corridor", number = 4, roomStatus = detectors, inventory = [], exits = [North 1, North 2, North 3
																						   ,South 5, South 6, South 7
																						   ], actors = []}
	room4		= {name = "room 4",   number = 5, roomStatus = detectors, inventory = [], exits = [North 4], actors = []}			
	room5		= {name = "room 5",   number = 6, roomStatus = detectors, inventory = [Blanket], exits = [North 4], actors = []}			
	room6		= {name = "room 6",   number = 7, roomStatus = detectors, inventory = [FireExtinguisher], exits = [North 4], actors = []}
	
	detectors = [FireDetector False,SmokeDetector False]			

:: Log			=	{ when		:: DateTime
					, who 		:: String
					, location  :: String
					, about		:: String
					}

myLog :: Shared [Log]
myLog = sharedStore "myLog" []

derive class iTask Log

addLog :: a b c -> Task () | toString a & toString b & toString c
addLog who location about
	=				 get currentDateTime
	>>= \dateTime -> upd (\log -> [{ who = (toString who), when = dateTime, location = toString location, about = toString about}:log]) myLog
	>>|				 return ()

showLog :: Task [Log]
showLog
	=				viewSharedInformation "Latest loggings..." [ViewWith (take 10)] myLog


// tasks on this specific MAP type

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "Adventure" myTasks)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
		, publish "/map"   (WebApp []) (\_-> showMap)
		, publish "/alarm" (WebApp []) (\_-> setRoomDetectors)
		, publish "/log"   (WebApp []) (\_-> showLog)
        ] world
 

myTasks :: [Workflow]
myTasks = 	[	workflow "crew member"	"enter map, walk around, follow instructions of commander"  (get currentUser >>= \me -> actorWithInstructions me)
			,	workflow "commander"	"give instructions to crew members on the map" 				giveInstructions			
		 	]

// tasks for an actor

actorWithInstructions :: User  -> Task ()
actorWithInstructions user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addActorToMap (newActor user) loc handleInstructions myMap

newActor user 			
	= {userName = user, carrying = [], actorStatus = {todo = [], occupied = Available}}
 
handleInstructions map room actor 
	= if (isEmpty actor.actorStatus.todo)
		(				viewInformation "ToDo list is empty" [] () 
			>>| 		return  actor
		)
		(				enterMultipleChoice "ToDo list" [] actor.actorStatus.todo 
			>>= \done -> adjustToDoList actor room actor.actorStatus.todo done
		)
where
	adjustToDoList actor room todo done
	# ntodo = removeMembers todo done
	= 		addLog actor.userName room.number ("Completed: " +++ toMultiLineText done)
	 >>|	return {actor & actorStatus = {todo = ntodo, occupied = if (isEmpty ntodo) Available actor.actorStatus.occupied}}

// task for commander

giveInstructions :: Task ()
giveInstructions 
	= forever
	  (		showAlerts
	  		||-
	  		(	selectSomeOne
	  			-&&-
	  			enterInformation "Define Instructions" []
			 )	
	  >>* 	[ OnAction  ActionOk (hasValue (\((roomNumber,actor),instructions) 
	 									-> 	assignInstructions actor instructions
	 													)
	 											)
            , OnAction  ActionCancel (always (return ()))
            ]
	 )

assignInstructions actor instructions
	= 	updActorStatus actor.userName (\st -> {st & occupied = Busy
	 										  , todo 	 = instructions ++ st.todo
	 										  }) myMap
	>>| addLog "Commander" actor.userName ("To Do :" +++ toMultiLineText instructions)

showAlerts
	=	whileUnchanged myMap 
			\mymap ->  let alerts = [ (number,detector)	\\ (number,detectors) <- allRoomStatus mymap
														,  detector <- detectors
														| isHigh detector]
						in if (isEmpty alerts)
							(viewInformation "No Alerts..." [] [])
							(viewInformation "ALERTS !!!" [] alerts)

selectSomeOne :: Task (Int,MyActor)
selectSomeOne 
	=		viewInformation "Select worker to give instructions to..." [] ()
	>>*		[	OnAction (Action "AnyOne" []) 			(always (selectSomeOneWith noRestriction))
			,	OnAction (Action "Free Available" [])   (always (selectSomeOneWith isAvailable))
			] 
	
noRestriction _   = True
isAvailable actor = actor.actorStatus.occupied ===  Available

selectSomeOneWith :: ((MyActor) -> Bool) -> Task (Int,MyActor) 
selectSomeOneWith pred
	=	whileUnchanged myMap 
			\mymap ->  enterChoice "Assign worker" [] [(i,actor) \\ (i,actor) <- findAllActors mymap | pred actor ]

// detection system

setRoomDetectors :: Task ()
setRoomDetectors 
	=				get myMap
	>>= \map ->		enterChoice "The detectors of which room do you want to set?" [] (allRoomNumbers map)
	>>= \nr	 -> 	getRoomStatus nr myMap
	>>= \status ->	updateInformation ("Set detectors in the room number: " <+++ nr) [] (fromJust status)
	>>*				[ OnAction ActionOk (hasValue (\status -> 		updRoomStatus nr (\_ -> status) myMap 
																>>|	addLog "Alert System" ("Room " <+++ nr) (toMultiLineText status)))
					, OnAction ActionCancel (always (return ()))
					]
	>>|				setRoomDetectors

// general map viewing

showMap = 		viewSharedInformation "Map Status" [] (mapRead (map 'DIS'.elems) myMap)
        -||
        (get myMap >>- \m -> viewInformation "Shortest path" [] (shortestPath (const 1) 1 7 m))

