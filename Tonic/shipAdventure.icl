module shipAdventure
 
import adventure
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt

:: MyMap		:== MAP RoomStatus Object ActorStatus
:: MyActor		:== Actor Object ActorStatus
:: MyFloor		:== Floor RoomStatus Object ActorStatus
:: MyRoom		:== Room RoomStatus Object ActorStatus

:: RoomStatus 	:==	[Detector] 
:: Detector		= FireDetector Bool 
				| SmokeDetector Bool
                | FloodDetector Bool
:: Object 		= 	FireExtinguisher
				| 	Blanket
				| 	Plug
:: ActorStatus	= 	{ occupied	:: Availability
					}
:: Availability	=	Available | NotAvailable | Busy  

:: Instruction	= GotoRoom Int
                | FightFireInRoom Int | InspectSmokeInRoom Int
                | PlugLeakInRoom Int | InspectLeakInRoom Int
:: Priority		=	NormalPriority | HighPriority | Urgent | Vital

derive class iTask Detector, Object, ActorStatus, Availability, Instruction, Priority

instance == Object 		where (==) o1 o2 = o1 === o2
instance == Instruction where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2

isHigh (FireDetector  b) = b
isHigh (SmokeDetector b) = b
isHigh (FloodDetector b) = b

mapImage :: !(MyMap, Int) *TagSource -> Image (MyMap, Int)
mapImage (m, _) tsrc
  #! (floors, tsrc) = mapSt floorImage m tsrc
  = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) floors) Nothing

floorImage :: !MyFloor *TagSource -> *(Image (MyMap, Int), *TagSource)
floorImage floor [(floorTag, uFloorTag) : tsrc]
  #! rooms = map (\xs -> beside (repeat AtMiddleY) [] (map roomImage xs) Nothing) floor
  #! floor = tag uFloorTag (above (repeat AtMiddleX) [] rooms Nothing)
  = (skewx (deg -35.0) floor, tsrc)

roomDim =: 48.0

myFontDef = normalFontDef "Arial" 10.0

roomImage :: !MyRoom -> Image (MyMap, Int)
roomImage {number, exits, roomStatus, actors, inventory}
  #! (northEs, eastEs, southEs, westEs, upEs, downEs) = foldr foldExit ([], [], [], [], [], []) exits
  #! widthMul       = toReal (max (max (length northEs) (length southEs)) 1)
  #! heightMul      = toReal (max (max (length eastEs) (length westEs)) 1)
  #! bg             = rect (px (roomDim * widthMul)) (px (roomDim * heightMul)) <@< { fill = toSVGColor "white" }
  #! statusBadges   = above (repeat AtMiddleX) [] (foldr mkStatusBadge [] roomStatus) Nothing
  #! actorBadges    = above (repeat AtMiddleX) [] (map mkActorBadge actors) Nothing
  #! inventoryBadge = if (length inventory > 0)
                        (badgeImage <@< { fill = toSVGColor "purple" })
                        (empty zero zero)
  #! roomNo         = text myFontDef (toString number)
  #! total          = overlay [(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtMiddleY), (AtLeft, AtBottom)] [] [statusBadges, actorBadges, roomNo, inventoryBadge] (Just bg)
  #! total          = total <@< { onclick = onClick number, local = False }
  = total
  where
  foldExit (North n) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([n : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit (East n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [n : eastEs], southEs, westEs, upEs, downEs)
  foldExit (South n) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [n : southEs], westEs, upEs, downEs)
  foldExit (West n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [n : westEs], upEs, downEs)
  foldExit (Up n)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [n : upEs], downEs)
  foldExit (Down n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [n : downEs])

  mkStatusBadge (FireDetector  True) acc = [badgeImage <@< { fill = toSVGColor "red"  } : acc]
  mkStatusBadge (SmokeDetector True) acc = [badgeImage <@< { fill = toSVGColor "grey" } : acc]
  mkStatusBadge (FloodDetector True) acc = [badgeImage <@< { fill = toSVGColor "blue" } : acc]
  mkStatusBadge _                    acc = acc

  mkActorBadge {actorStatus = {occupied}} = badgeImage <@< { fill = toSVGColor (case occupied of
                                                                                  Available    -> "green"
                                                                                  NotAvailable -> "red"
                                                                                  Busy         -> "orange")}

  onClick number _ (m, _) = (m, number)

badgeImage = rect (px 8.0) (px 8.0) <@< { stroke = toSVGColor "black" }
                                    <@< { strokewidth = px 1.0 }

myMap  :: Shared MyMap
myMap = sharedStore "myBuilding" [floor0]
where
	floor0  	= [ [room1, room2, room3]
                  , [corridor]
                  , [room4, room5, room6]
                  ]
	room1		= {name = "room 1",   number = 1, roomStatus = detectors, inventory = [], exits = [South 4], actors = []}			
	room2		= {name = "room 2",   number = 2, roomStatus = detectors, inventory = [], exits = [South 4], actors = []}			
	room3		= {name = "room 3",   number = 3, roomStatus = detectors, inventory = [FireExtinguisher], exits = [South 4], actors = []}
	corridor	= {name = "corridor", number = 4, roomStatus = detectors, inventory = [], exits = [North 1, North 2, North 3
																						   ,South 5, South 6, South 7
																						   ], actors = []}
	room4		= {name = "room 5",   number = 5, roomStatus = detectors, inventory = [], exits = [North 4], actors = []}			
	room5		= {name = "room 6",   number = 6, roomStatus = detectors, inventory = [Blanket], exits = [North 4], actors = []}			
	room6		= {name = "room 7",   number = 7, roomStatus = detectors, inventory = [FireExtinguisher], exits = [North 4], actors = []}
	
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
myTasks = 	[	workflow "walk around"	"enter map, walk around, follow instructions of commander"  (get currentUser >>= \me -> actorWithInstructions me)
			,	workflow "commander"	"give instructions to crew members on the map" 				giveInstructions			
		 	]

// tasks for an actor

actorWithInstructions :: User  -> Task ()
actorWithInstructions user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addActorToMap (newActor user) loc myMap

newActor user 			
	= {userName = user, carrying = [], actorStatus = {occupied = Available}}
 
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

assignInstructions :: MyActor [(Instruction,Priority)] -> Task ()
assignInstructions actor instructions
	= 	updActorStatus actor.userName (\st -> {st & occupied = Busy}) myMap
	>>| addLog "Commander" actor.userName ("To Do :" +++ toMultiLineText instructions)
	>>| addTasks actor instructions

addTasks :: MyActor [(Instruction,Priority)]  -> Task ()
addTasks actor [] = return ()
addTasks actor [(GotoRoom n,prio) : ins]
		=		addLog "Commander" actor.userName ("New task: GotoRoom" <+++ n)	
		>>|		addTaskWhileWalking actor.userName (gotoTask n) myMap >>| addTasks actor ins
addTasks actor [(InspectSmokeInRoom n,prio) : ins]
		=	addTaskWhileWalking actor.userName (gotoTask n) myMap >>| addTasks actor ins
addTasks actor [(FightFireInRoom n,prio) : ins]
		=	addTaskWhileWalking actor.userName (gotoTask n) myMap >>| addTasks actor ins

gotoTask :: Int MyActor MyRoom MyMap -> Task Bool
gotoTask nr curActor curRoom curMap
		=	(viewInformation ("I need to go to room number " <+++ nr) []  () @! False)
			-||-  
			if (curRoom.number == nr) 
					(	addLog curActor.userName curRoom.number ("Goto stopped, reached room: " <+++ nr)
					>>| return True
					)
				 	(viewInformation ("I am currently in room " <+++ curRoom.number) [] () @! False)
			-||-
			(viewInformation ("Shortest path from room " <+++ curRoom.number <+++
							 " to room " <+++ nr <+++ 
							 " is " <+++ shortestPath (const 1) curRoom.number nr curMap) [] () @! False)


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

showMap = updateInformationWithShared "Map Status" [imageUpdate id mapImage (\_ _ -> Nothing) (const snd)] myMap -1
            >&> withSelection (return ())
                  (\selRoom -> viewSharedInformation "Room status" [ViewWith (getRoomFromMap selRoom)] myMap)
