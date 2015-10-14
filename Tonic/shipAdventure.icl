module shipAdventure
 
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray

import adventure

:: MyMap		:== MAP 	RoomStatus Object ActorStatus
:: MyActor		:== Actor 	Object ActorStatus
:: MyFloor		:== Floor 	RoomStatus Object ActorStatus
:: MyRoom		:== Room 	RoomStatus Object ActorStatus

:: RoomStatus 	:==	[Detector] 
:: Detector		= 	FireDetector Bool 
				| 	SmokeDetector Bool
                | 	FloodDetector Bool
:: Object 		= 	FireExtinguisher
				| 	Blanket
				| 	Plug
:: ActorStatus	= 	{ occupied	:: Availability
					}
:: Availability	=	Available | NotAvailable | Busy  

:: Instruction	= 	FightFireInRoom Int Object
                | 	InspectSmokeInRoom Int
                | 	PlugLeakInRoom Int  Object
                | 	InspectLeakInRoom Int
:: Priority		=	Low | Normal | High | Highest

:: Log			=	{ when		:: DateTime
					, who 		:: String
					, location  :: String
					, about		:: String
					}

derive class iTask Detector, Object, ActorStatus, Availability, Instruction, Priority, Log

instance == Object 		where (==) o1 o2 = o1 === o2
instance == Instruction where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2

instance toString Detector
where toString (FireDetector _)  = "Fire Alarm"
	  toString (SmokeDetector _) = "Smoke Alarm"
	  toString (FloodDetector _) = "Flood Alarm"

// shared stores:

myMap  :: Shared MyMap						// map of the ship
myMap = sharedStore "myBuilding" myShip

myLog :: Shared [Log]						// logging events					
myLog = sharedStore "myLog" []

// main tasks

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

// initial task to place an actor on the map
// one can only assign tasks to actors on the map

actorWithInstructions :: User  -> Task ()
actorWithInstructions user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addActorToMap (newActor user) loc myMap
where
	newActor user 			
		= {userName = user, carrying = [], actorStatus = {occupied = Available}}
 
// given the alarms one has to decide which tasks to assign to handle the situation

giveInstructions :: Task ()
giveInstructions 
	= 				get currentUser
		>>= \me ->  forever
					(							showAlarms 
						>>= \alarms -> 			if (isEmpty alarms) (return ())
						(						enterChoice "Choose which Alarm to handle : " [ChooseWith (ChooseFromRadioButtons id)] alarms 
						>>= \(location,alarm) -> ( 	selectSomeOneToHandle (location,alarm)
 													-&&-
 													selectObject (location,alarm)
 													-&&- 		
												  	updateChoice "Select the Priority : " [ChooseWith (ChooseFromRadioButtons id)] [Low, Normal, High, Highest] High
												)
						>>* 					[ OnAction  ActionOk     (ifValue isMatching (handleAlert me (location,alarm)))
						       					, OnAction  ActionCancel (always (return ()))
						       					]
						)
	 				)

isMatching ((k,actor),(mbobject,priority)) = True
isMatching _ = False

handleAlert user (i,FireDetector b) ((k,actor),(Just (location,object),priority))
# instruction = FightFireInRoom i FireExtinguisher
= 		updActorStatus actor.userName (\st -> {st & occupied = Busy}) myMap
 >>|	addLog "Commander" actor.userName ("Instruction:" <+++ instruction)
 >>| 	addTaskWhileWalking user actor.userName ("Fight Fire in Room " <+++ i) (toSingleLineText priority) (handleFireTask instruction location) myMap
handleAlert _ _ _ = return ()

showAlarms :: Task [(RoomNumber,Detector)]
showAlarms
	=	whileUnchanged myMap 
			(\curMap ->  let alarms = [ (number,detector)	\\ (number,detectors) <- allRoomStatus curMap
														,  detector <- detectors
														| isHigh detector]
						 in if (isEmpty alarms)
								(viewInformation "No alarms..." [] [])
								(viewInformation "ALARM!!!, press Continue to react..." [ViewWith (map (\(i,a) -> "There is a *" <+++ a <+++ "* in Room : " <+++ i))] alarms)
						>>| return alarms)

selectSomeOneToHandle :: (RoomNumber,Detector) -> Task (Int,MyActor)
selectSomeOneToHandle (number,detector)
	=	whileUnchanged myMap 
			(\curMap ->  enterChoice ("Who should handle the *" <+++ toString detector <+++ "* in Room : " <+++ number) [] 
							(findAllActors curMap))


selectObject :: (RoomNumber,Detector) -> Task (Maybe (RoomNumber,Object))
selectObject (i,FireDetector _)
	= whileUnchanged myMap 
			\curMap -> 	enterChoice "Fight Fire with : " [] (fireFightObjects curMap) @ (\object -> Just object)
selectObject (i,SmokeDetector _) 
	= return Nothing
selectObject (i,FloodDetector _)  
	= whileUnchanged myMap 
			\curMap -> 	enterChoice "Fight Fire with : " [] (waterFightObjects curMap)  >>= \object -> return (Just object)

waterFightObjects map	= [(i,Plug)   \\ (i,Plug)   <- findAllObjects map ]
fireFightObjects  map	= [(i,object) \\ (i,object) <- findAllObjects map | object == FireExtinguisher || object == Blanket]

handleFireTask :: Instruction RoomNumber MyActor MyRoom MyMap -> Task Bool
handleFireTask (FightFireInRoom nr FireExtinguisher) fnr curActor curRoom curMap
	=		viewInformation ("Fight Fire in Room : " <+++ nr <+++ " With extinguiser of room " <+++ fnr) []  () @! False

               
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
							 " is " <+++ shipShortestPath curRoom.number nr curMap) [] () @! False)

// general map viewing

showMap = updateInformationWithShared "Map Status" [imageUpdate id mapImage (\_ _ -> Nothing) (const snd)] myMap -1
            >&> withSelection (return ())
                  (\selRoom -> viewSharedInformation "Room status" [ViewWith (getRoomFromMap selRoom)] myMap)

// setting and resetting of the detection systems

setRoomDetectors :: Task ()
setRoomDetectors 
	=				get myMap
	>>= \curMap ->		enterChoice "The detectors of which room do you want to set?" [] (allRoomNumbers curMap)
	>>= \nr	 -> 	getRoomStatus nr myMap
	>>= \status ->	updateInformation ("Set detectors in the room number: " <+++ nr) [] (fromJust status)
	>>*				[ OnAction ActionOk (hasValue (\status -> 		updRoomStatus nr (\_ -> status) myMap 
																>>|	addLog "Alarm Set/Reset" ("Room " <+++ nr) (toMultiLineText status)))
					, OnAction ActionCancel (always (return ()))
					]
	>>|				setRoomDetectors

// Logging events

addLog :: a b c -> Task () | toString a & toString b & toString c
addLog who location about
	=				 get currentDateTime
	>>= \dateTime -> upd (\log -> [{ who = (toString who), when = dateTime, location = toString location, about = toString about}:log]) myLog
	>>|				 return ()

showLog :: Task [Log]
showLog
	=				viewSharedInformation "Latest loggings..." [ViewWith (take 10)] myLog

// end of task definitions -------------------------------------------------------------------------------------------------------

// definition of the ship layout

myShip =[floor0]
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

// utility functions .....

isHigh (FireDetector  b) = b
isHigh (SmokeDetector b) = b
isHigh (FloodDetector b) = b

// shortest path given the alarms set on the ship

shipShortestPath startRoomNumber endRoomNumber allRooms = shortestPath cost startRoomNumber endRoomNumber allRooms
  where
  cost detectors = 1 + sum (map detectorCost detectors)
  detectorCost (FireDetector  True) = 50
  detectorCost (SmokeDetector True) = 25
  detectorCost (FloodDetector True) = 50
  detectorCost _                    = 0

// making an image from the map ...

mapImage :: !(MyMap, Int) *TagSource -> Image (MyMap, Int)
mapImage (m, _) tsrc
  #! (floors, tsrc) = mapSt floorImage m tsrc
  #! allFloors      = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) floors) Nothing
  #! legendElems    = [ (mkStatusBadgeBackground (FireDetector True), "Fire detected")
                      , (mkStatusBadgeBackground (SmokeDetector True), "Smoke detected")
                      , (mkStatusBadgeBackground (FloodDetector True), "Flood detected")
                      , (mkActorBadgeBackground Available, "Available person")
                      , (mkActorBadgeBackground NotAvailable, "Unavailable person")
                      , (mkActorBadgeBackground Busy, "Busy person")
                      , (mkInventoryBadge [], "Room inventory")
                      ]
  #! legendElems    = map (\(img, descr) -> beside (repeat AtMiddleY) [] [img, text myFontDef (" " +++ descr)] Nothing) legendElems
  #! legend         = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) legendElems) Nothing
  = beside [] [] [allFloors, empty (px 8.0) (px 8.0), legend] Nothing

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
                        (mkInventoryBadge inventory)
                        (empty zero zero)
  #! roomNo         = text myFontDef (toString number)
  #! total          = overlay [(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtMiddleY), (AtLeft, AtBottom)]
                              [(px 2.5, px 2.5), (px -2.5, px 2.5), (zero, zero), (px 2.5, px -2.5)]
                              [statusBadges, actorBadges, roomNo, inventoryBadge] (Just bg)
  #! total          = total <@< { onclick = onClick number, local = False }
  = total
  where
  foldExit (North n) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([n : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit (East n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [n : eastEs], southEs, westEs, upEs, downEs)
  foldExit (South n) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [n : southEs], westEs, upEs, downEs)
  foldExit (West n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [n : westEs], upEs, downEs)
  foldExit (Up n)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [n : upEs], downEs)
  foldExit (Down n)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [n : downEs])

  onClick number _ (m, _) = (m, number)

mkStatusBadge d acc
  | isHigh d  = [mkStatusBadgeBackground d : acc]
  | otherwise = acc

mkStatusBadgeBackground (FireDetector  _) = badgeImage <@< { fill = toSVGColor "red"  }
mkStatusBadgeBackground (SmokeDetector _) = badgeImage <@< { fill = toSVGColor "grey" }
mkStatusBadgeBackground (FloodDetector _) = badgeImage <@< { fill = toSVGColor "lightblue" }

mkActorBadge {actorStatus = {occupied}, userName, carrying}
  #! actorBadge  = mkActorBadgeBackground occupied
  #! userStr     = toString userName
  #! userInitial = text myFontDef (userStr % (0,0)) <@< { fill = toSVGColor "white" }
  #! actorBadge  = overlay [(AtMiddleX, AtMiddleY)] [] [userInitial] (Just actorBadge)
  #! inventory   = if (length carrying > 0)
                     [mkInventoryBadge carrying]
                     []
  = above (repeat AtMiddleX) [] [actorBadge : inventory] Nothing

mkActorBadgeBackground occupied = badgeImage <@< { fill = toSVGColor (case occupied of
                                                                        Available    -> "green"
                                                                        NotAvailable -> "black"
                                                                        Busy         -> "orange")}

mkInventoryBadge xs
  #! badge = badgeImage <@< { fill = toSVGColor "purple" }
  #! txt   = text myFontDef (toString (length xs)) <@< { fill = toSVGColor "white" }
  = overlay [(AtMiddleX, AtMiddleY)] [] [txt] (Just badge)

badgeImage = rect (px 10.0) (px 10.0) <@< { stroke = toSVGColor "black" }
                                      <@< { strokewidth = px 1.0 }