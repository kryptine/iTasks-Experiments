module shipAdventure
 
import adventure
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray

:: MyMap		:== MAP RoomStatus Object ActorStatus
:: MyActor		:== Actor Object ActorStatus
:: MyFloor		:== Floor RoomStatus Object ActorStatus
:: MyRoom		:== Room RoomStatus Object ActorStatus

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

derive class iTask Detector, Object, ActorStatus, Availability, Instruction, Priority

instance == Object      where (==) o1 o2 = o1 === o2
instance == Instruction where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2

shipShortestPath startRoomNumber endRoomNumber allRooms = shortestPath cost startRoomNumber endRoomNumber allRooms
  where
  cost detectors = 1 + sum (map detectorCost detectors)
  detectorCost (FireDetector  True) = 1000
  detectorCost (SmokeDetector True) = 250
  detectorCost (FloodDetector True) = 1000
  detectorCost _                    = 0

isHigh (FireDetector  b) = b
isHigh (SmokeDetector b) = b
isHigh (FloodDetector b) = b

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
  foldExit (North n, _) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([n : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit (East n, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [n : eastEs], southEs, westEs, upEs, downEs)
  foldExit (South n, _) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [n : southEs], westEs, upEs, downEs)
  foldExit (West n, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [n : westEs], upEs, downEs)
  foldExit (Up n, _)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [n : upEs], downEs)
  foldExit (Down n, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [n : downEs])

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

myMap  :: Shared MyMap
myMap = sharedStore "myBuilding" [floor0]
where
	floor0  	= [ [room1, room2, room3]
                  , [corridor]
                  , [room4, room5, room6]
                  ]
	room1		= {name = "room 1",   number = 1, roomStatus = detectors, inventory = [], exits = [(South 4, False)], actors = []}			
	room2		= {name = "room 2",   number = 2, roomStatus = detectors, inventory = [], exits = [(South 4, False)], actors = []}			
	room3		= {name = "room 3",   number = 3, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(South 4, False)], actors = []}
	corridor	= {name = "corridor", number = 4, roomStatus = detectors, inventory = [], exits = [(North 1, False), (North 2, False), (North 3, False)
																						   , (South 5, False), (South 6, False), (South 7, False)
																						   ], actors = []}
	room4		= {name = "room 5",   number = 5, roomStatus = detectors, inventory = [], exits = [(North 4, False)], actors = []}			
	room5		= {name = "room 6",   number = 6, roomStatus = detectors, inventory = [Blanket], exits = [(North 4, False)], actors = []}			
	room6		= {name = "room 7",   number = 7, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 4, False)], actors = []}
	
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
	= 				get currentUser
		>>= \me ->  forever(	(					showAlerts 
								 >>= \alerts -> 	get myMap
								 >>= \map ->		((enterChoice "Handle Alarm : " [] alerts 
								 					-&&-
													enterChoice "Using Object : " [] (findAllObjects map))
													-&&-
													(enterChoice "With Priority" [] [Low, Normal, High, Highest]
													-&&-
													selectSomeOneWith noRestriction))
	  							)
	  						>>* [ OnAction  ActionOk     (ifValue isMatching (handleAlert me))
						        , OnAction  ActionCancel (always (return ()))
						        ]
	 						)

isMatching (((i,FireDetector  b),(j,FireExtinguisher)),(priority,(k,actor))) = True
isMatching _ = False

handleAlert user (((i,FireDetector  b),(j,FireExtinguisher)),(priority,(k,actor)))
# instruction = FightFireInRoom i FireExtinguisher
= 		updActorStatus actor.userName (\st -> {st & occupied = Busy}) myMap
 >>|	addLog "Commander" actor.userName ("Instruction:" <+++ instruction)
 >>| 	addTaskWhileWalking user actor.userName ("Fight Fire in Room " <+++ i) (toSingleLineText priority) (handleFireTask instruction j) myMap
handleAlert _ _ = return ()



handleFireTask :: Instruction RoomNumber MyActor MyRoom MyMap -> Task Bool
handleFireTask (FightFireInRoom nr FireExtinguisher) fnr curActor curRoom curMap
	=		viewInformation ("Fight Fire in Room : " <+++ nr <+++ "with extinguiser of room " <+++ fnr) []  () @! False

/*
makeList [] = []
makeList [(n,FireDetector  b):alerts] = [
makeList [(n,SmokeDetector b):alerts]
makeList [(n,FloodDetector b):alerts]
FightFireInRoom Int Object
                | 	InspectSmokeInRoom Int
                | 	PlugLeakInRoom Int  Object
                | 	InspectLeakInRoom Int
            
*/                
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


showAlerts :: Task [(RoomNumber,Detector)]
showAlerts
	=	whileUnchanged myMap 
			\mymap ->  let alerts = [ (number,detector)	\\ (number,detectors) <- allRoomStatus mymap
														,  detector <- detectors
														| isHigh detector]
						in if (isEmpty alerts)
							(viewInformation "No Alerts..." [] [])
							(viewInformation "ALERTS !!!" [] alerts)
						>>| return alerts

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
			\mymap ->  enterChoice "Assign Actor" [] [(i,actor) \\ (i,actor) <- findAllActors mymap | pred actor ]

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
