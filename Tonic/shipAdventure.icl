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

:: MapClick     = NoMapClick | SelectRoom RoomNumber | ToggleAlarm RoomNumber Detector

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

:: Instruction	= 	FightFireInRoom Int 
                | 	InspectSmokeInRoom Int
                | 	PlugLeakInRoom Int  
                | 	InspectLeakInRoom Int
:: Priority		=	Low | Normal | High | Highest

:: Log			=	{ when		:: DateTime
					, who 		:: String
					, location  :: String
					, about		:: String
					}

derive class iTask Detector, Object, ActorStatus, Availability, Instruction, Priority, Log, MapClick

instance == Object      where (==) o1 o2 = o1 === o2
instance == Instruction where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2
instance toString Object where
  toString FireExtinguisher = "Fire extinguiser"
  toString Blanket          = "Blanket"
  toString Plug             = "Plug"

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
	>>= \loc ->	addActorToMap mkRoom (newActor user) loc myMap
where
	newActor user 			
		= {userName = user, carrying = [], actorStatus = {occupied = Available}}
 
// given the alarms one has to decide which tasks to assign to handle the situation

giveInstructions :: Task ()
giveInstructions 
=	forever 
	(			get currentUser
	>>= \me ->  						showAlarms 
					>>= \alarms -> 		(							enterChoice "Choose which Alarm to handle : " [ChooseWith (ChooseFromRadioButtons showAlarm)] alarms
											>&> 					withSelection (viewInformation () [] "No Alarm Selected")
								 			\(alarmLoc,detector) -> selectSomeOneToHandle (alarmLoc,detector)
								 			>&>						withSelection (viewInformation () [] "No Crew Member Selected")
								 			\(actorLoc,actor) ->	viewObject (actorLoc,actor) (alarmLoc,detector)
								 			>&>						withSelection (viewInformation () [] "")
											\_ ->					updateChoice "Select the Priority : " [ChooseWith (ChooseFromRadioButtons id)] [Low, Normal, High, Highest] High
											>>* 					[ OnAction  ActionOk     (hasValue (\prio -> return (me,(alarmLoc,detector),(actorLoc,actor),prio)))
					       											]
					       				)
					>>- \result ->		handleAlarm result
	)

handleAlarm (me,(alarmLoc,detector),(actorLoc,actor),priority)
# instruction = FightFireInRoom alarmLoc 
= 		updActorStatus actor.userName (\st -> {st & occupied = Busy}) myMap
 >>|	addLog "Commander" actor.userName ("Instruction:" <+++ instruction)
 >>| 	appendTopLevelTaskPrioFor me ("Report on Fire in " <+++ alarmLoc) "High" True 
 			(addTaskWhileWalking actor ("Fight Fire in " <+++ alarmLoc) (toSingleLineText priority) 
 			 (handleFireTask instruction) ) @! ()

handleAlarm _  = return ()

addTaskWhileWalking :: MyActor String String (MyActor MyRoom MyMap -> Task (Maybe a)) -> Task () | iTask a
addTaskWhileWalking actor title priority task 
	=					(((actor.userName,title)  @: moveAround mkRoom actor (Just task) myMap) 
						-||-
						(viewInformation ("Cancel task \"" <+++ title <+++ "\"") [] () @! Nothing))
	>>= \mba ->	if (isNothing mba)
	 					(viewInformation ("Task " <+++ title <+++ " has been cancelled by you") [] ())
						(viewInformation ("Task " <+++ title <+++ " terminated normally, returning:") [] (fromJust mba) @! ())
	>>|			return ()

handleFireTask :: Instruction MyActor MyRoom MyMap -> Task (Maybe String)
handleFireTask (FightFireInRoom nr) curActor curRoom curMap
	=		(viewInformation ("Fight Fire in Room: " <+++ nr) []  ()
			-||- 
			viewInformation ("Shortest Path to Fire: " <+++ shipShortestPath curRoom.number nr curMap) [] ()
			-||-
			let (_,distExt) = statResource FireExtinguisher curRoom.number curMap in
				viewInformation ("Shortest Path to a FireExtinguiser: " <+++ distExt) [] ()
			-||-
			let (_,distBlanket) = statResource Blanket curRoom.number curMap in
				viewInformation ("Shortest Path to a Blanket: " <+++ distBlanket) [] ()
			)
			>>* [OnAction (Action "Fire Extinguished" []) (ifCond (curRoom.number == nr) (return (Just "Fire Extinguised")))
				,OnAction (Action "Need More Help" []) (always (return (Just "I need more help...")))
				]
	

mkRoom :: MyRoom -> Task ()
mkRoom room = updateInformationWithShared "Room Status" [imageUpdate id (\(mp, _) -> roomImage True (Just room)) (\_ _ -> Nothing) (const snd)] myMap NoMapClick @! ()

showAlarms :: Task [(RoomNumber,Detector)]
showAlarms
	=	whileUnchanged myMap 
			(\curMap ->  let alarms = [ (number,detector)	\\ (number,detectors) <- allRoomStatus curMap
														,  detector <- detectors
														| isHigh detector]
						 in if (isEmpty alarms)
								(viewInformation "You have nothing yet to worry about ... Waiting for alarms to go off..." [] [])
								(viewInformation "There are ALARMS !!!" [ViewWith (map showAlarm)] alarms)
						>>| return alarms)

showAlarm (alarmLoc,detector)
	= "Room : " <+++ alarmLoc <+++ " : " <+++ toString detector <+++ " !!! "

selectSomeOneToHandle :: (RoomNumber,Detector) -> Task (Int,MyActor)
selectSomeOneToHandle (number,detector)
	=	whileUnchanged myMap 
			(\curMap ->  let allActors = findAllActors curMap in
							enterChoice (if (isEmpty allActors) "No one available at all !: : " "Who should handle: " <+++ showAlarm (number,detector)) [] allActors)

isMatching ((k,actor),(mbobject,priority)) = True
isMatching _ = False

viewObject :: (RoomNumber,MyActor) (RoomNumber,Detector) -> Task ()
viewObject (actorLoc,actor) (alarmLoc,FireDetector _)
	= whileUnchanged myMap 
			\curMap -> 	let		(nrExt,(distExt,_)) 			= statResource FireExtinguisher actorLoc curMap
								(nrBlankets,(distBlankets,_)) 	= statResource Blanket 			actorLoc curMap
						in	viewInformation ("Distances Between " <+++ actor.userName <+++ " and Currently Available Resources") [] 
								[ "The Fire Detected in room " <+++ alarmLoc <+++ ": " <+++ length (shipShortestPath actorLoc alarmLoc curMap) <+++ " Rooms Away."
								, "Closest Extinquisher: " <+++ distExt <+++ " Rooms Away."
								, "Closest Blanket: " <+++ distBlankets <+++ " Rooms Away."
								, "Available Extinquishers: "  <+++ nrExt 
								, "Available Blankets: "  <+++ nrBlankets 
								] @! ()

viewObject (actorLoc,actor) (alarmLoc,SmokeDetector _) 
	= whileUnchanged myMap 
			\curMap ->		viewInformation ("Distances Between " <+++ actor.userName <+++ " and Currently Available Resources") [] 
								[ "Distance " <+++ SmokeDetector <+++ " : " <+++ length (shipShortestPath actorLoc alarmLoc curMap) <+++ " Rooms Away."
								] @! ()
viewObject (actorLoc,actor) (alarmLoc,FloodDetector _)  
	= whileUnchanged myMap 
			\curMap -> 	let	(nrPlugs,(distPlugs,_)) 		= statResource Plug actorLoc curMap
						in	viewInformation ("Distances Between " <+++ actor.userName <+++ " and Currently Available Resources") [] 
								[ "The Flood Detected in Room: " <+++ alarmLoc <+++ " is " <+++ length (shipShortestPath actorLoc alarmLoc curMap) <+++ " Rooms Away."
								, "Closest Plug: " <+++ distPlugs <+++ " Rooms Away."
								, "Available Plugs: "  <+++ nrPlugs 
								] @! ()

statResource :: Object RoomNumber MyMap -> (Int,(Int,[Exit]))
statResource kind actorLoc curMap
	= (numberResources, if (numberResources == 0) (-1,[]) (hd spath))
	where
		numberResources = length spath 
		spath = sortBy (\(i,p1) (j,p2) -> i < j)   [let path = shipShortestPath actorLoc objectLoc curMap in (length path, path) 
													\\ (objectLoc,found) <- findAllObjects curMap | found == kind ]


// general map viewing

showMap :: Task MapClick
showMap = updateInformationWithShared "Map Status" [imageUpdate id (mapImage False) (\_ _ -> Nothing) (const snd)] myMap NoMapClick
            >&> withSelection (return ())
                  (\mapClick -> case mapClick of
                                  SelectRoom selRoom -> updateInformationWithShared "Room Status" [imageUpdate id (\(mp, _) -> roomImage True (getRoomFromMap selRoom mp)) (\_ _ -> Nothing) (const snd)] myMap NoMapClick
                                  _ -> return NoMapClick)

// setting and resetting of the detection systems

detectorEq (FireDetector _) (FireDetector _)   = True
detectorEq (SmokeDetector _) (SmokeDetector _) = True
detectorEq (FloodDetector _) (FloodDetector _) = True
detectorEq _ _ = False

setRoomDetectors :: Task ()
setRoomDetectors 
	= updateInformationWithShared "Map Status" [imageUpdate id (mapImage True) (\_ _ -> Nothing) (const snd)] myMap NoMapClick
      >>* [OnValue (\tv -> case tv of
                             Value (ToggleAlarm selRoom d) _ -> Just (updRoomStatus selRoom (updDetector d) myMap >>| setRoomDetectors)
                             _ -> Nothing
                   )]
    where
    updDetector :: !Detector !RoomStatus -> RoomStatus
    updDetector d r = [if (detectorEq d d`) (toggleDetector d`) d` \\ d` <- r]

toggleDetector :: !Detector -> Detector
toggleDetector (FireDetector  b) = FireDetector (not b)
toggleDetector (SmokeDetector b) = SmokeDetector (not b)
toggleDetector (FloodDetector b) = FloodDetector (not b)

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

myShip = [floor0, floor1]
where
	floor0  	= [ [back0]
                  , [room01, room02, room03]
                  , [corridor0]
                  , [room04, room05, room06]
                  , [room07, room08]
                  , [front0]
                  ]
	back0		= {name = "front 0",    number = 1,  roomStatus = detectors, inventory = [], exits = [(South 2, False), (South 3, False), (South 4, False), (Down 12, False)], actors = []}
	room01		= {name = "room 0.1",   number = 2,  roomStatus = detectors, inventory = [], exits = [(North 1, False), (South 5, False)], actors = []}			
	room02		= {name = "room 0.2",   number = 3,  roomStatus = detectors, inventory = [], exits = [(North 1, False), (South 5, False)], actors = []}			
	room03		= {name = "room 0.3",   number = 4,  roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 1, False), (South 5, False)], actors = []}
	corridor0	= {name = "corridor 0", number = 5,  roomStatus = detectors, inventory = [], exits = [(North 2, False), (North 3, False), (North 4, False)
                                                                                                     , (South 6, False), (South 7, False), (South 8, False)
                                                                                                     , (Down 16, False)
                                                                                                     ], actors = []}
	room04		= {name = "room 0.4",   number = 6,  roomStatus = detectors, inventory = [], exits = [(North 5, False), (South 9, False)], actors = []}			
	room05		= {name = "room 0.5",   number = 7,  roomStatus = detectors, inventory = [Blanket], exits = [(North 5, False), (South 9, False), (South 10, False)], actors = []}			
	room06		= {name = "room 0.6",   number = 8,  roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 5, False), (South 10, False)], actors = []}
	room07		= {name = "room 0.7",   number = 9,  roomStatus = detectors, inventory = [Blanket], exits = [(North 6, False), (North 7, False), (South 11, False)], actors = []}			
	room08		= {name = "room 0.8",   number = 10, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 7, False), (North 8, False), (South 11, False)], actors = []}
	front0		= {name = "back 0",     number = 11, roomStatus = detectors, inventory = [], exits = [(North 9, False), (North 10, False), (Down 22, False)], actors = []}

	floor1  	= [ [back1]
                  , [room11, room12, room13]
                  , [corridor1]
                  , [room14, room15, room16]
                  , [room17, room18]
                  , [front1]
                  ]
	back1		= {name = "front 1",    number = 12, roomStatus = detectors, inventory = [], exits = [(South 13, False), (South 14, False), (South 15, False), (Up 1, False)], actors = []}
	room11		= {name = "room 1.1",   number = 13, roomStatus = detectors, inventory = [], exits = [(North 12, False), (South 16, False)], actors = []}
	room12		= {name = "room 1.2",   number = 14, roomStatus = detectors, inventory = [], exits = [(North 12, False), (South 16, False)], actors = []}
	room13		= {name = "room 1.3",   number = 15, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 12, False), (South 16, False)], actors = []}
	corridor1	= {name = "corridor 1", number = 16, roomStatus = detectors, inventory = [], exits = [(North 13, False), (North 14, False), (North 15, False)
                                                                                                     , (South 17, False), (South 18, False), (South 19, False)
                                                                                                     , (Up 5, False)
                                                                                                     ], actors = []}
	room14		= {name = "room 1.4",   number = 17, roomStatus = detectors, inventory = [], exits = [(North 16, False), (South 20, False)], actors = []}
	room15		= {name = "room 1.5",   number = 18, roomStatus = detectors, inventory = [Blanket], exits = [(North 16, False), (South 20, False), (South 21, False)], actors = []}
	room16		= {name = "room 1.6",   number = 19, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 16, False), (South 21, False)], actors = []}
	room17		= {name = "room 1.7",   number = 20, roomStatus = detectors, inventory = [Blanket], exits = [(North 17, False), (North 18, False), (South 22, False)], actors = []}
	room18		= {name = "room 1.8",   number = 21, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 18, False), (North 19, False), (South 22, False)], actors = []}
	front1		= {name = "back 1",     number = 22, roomStatus = detectors, inventory = [], exits = [(North 20, False), (North 21, False), (Up 11, False)], actors = []}
	
	detectors = [FireDetector False,SmokeDetector False,FloodDetector False]

// utility functions .....
isHigh :: !Detector -> Bool
isHigh (FireDetector  b) = b
isHigh (SmokeDetector b) = b
isHigh (FloodDetector b) = b

// shortest path given the alarms set on the ship

shipShortestPath startRoomNumber endRoomNumber allRooms = shortestPath cost startRoomNumber endRoomNumber allRooms
  where
  cost detectors = 1 + sum (map detectorCost detectors)
  detectorCost (FireDetector  True) = 1000
  detectorCost (SmokeDetector True) = 250
  detectorCost (FloodDetector True) = 1000
  detectorCost _                    = 0

// making an image from the map ...

mapImage :: !Bool !(!MyMap, MapClick) !*TagSource -> Image (!MyMap, MapClick)
mapImage mngmnt (m, _) tsrc
  #! (floors, tsrc) = mapSt (floorImage mngmnt) (zip2 m (reverse [0..length m])) tsrc
  #! allFloors      = beside (repeat AtMiddleY) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) floors) Nothing
  #! legendElems    = [ (mkStatusBadgeBackground (FireDetector True), "Fire detected")
                      , (mkStatusBadgeBackground (SmokeDetector True), "Smoke detected")
                      , (mkStatusBadgeBackground (FloodDetector True), "Flood detected")
                      , (mkActorBadgeBackground Available, "Available person")
                      , (mkActorBadgeBackground NotAvailable, "Unavailable person")
                      , (mkActorBadgeBackground Busy, "Busy person")
                      , (mkInventoryBadgeBackground, "Room inventory")
                      , (mkUpDown (Up 0, False), "Staircase up")
                      , (mkUpDown (Down 0, False), "Staircase down")
                      ]
  #! legendElems    = map (\(img, descr) -> beside (repeat AtMiddleY) [] [img, text myFontDef (" " +++ descr)] Nothing) legendElems
  #! legend         = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) legendElems) Nothing
  = beside [] [] [allFloors, empty (px 8.0) (px 8.0), legend] Nothing

floorImage :: !Bool !(!MyFloor, !Int) !*TagSource -> *(!Image (!MyMap, MapClick), !*TagSource)
floorImage mngmnt (floor, floorNo) [(floorTag, uFloorTag) : tsrc]
  #! (rooms, tsrc) = mapSt f floor tsrc
  #! floor         = tag uFloorTag (above (repeat AtMiddleX) [] [text myFontDef ("Deck " +++ toString floorNo) : rooms] Nothing)
  = (floor, tsrc)
  where
  f :: ![MyRoom] !*TagSource -> *(!Image (!MyMap, MapClick), !*TagSource)
  f rooms tsrc
    #! (rooms`, tsrc) = mapSt (roomImage` mngmnt False) rooms tsrc
    = (beside (repeat AtMiddleY) [] rooms` Nothing, tsrc)

roomDim =: 64.0
exitWidth =: 12.0

myFontDef = normalFontDef "Arial" 10.0

roomImage :: !Bool !(Maybe MyRoom) !*TagSource -> Image (!MyMap, !MapClick)
roomImage zoomed (Just room) tsrc = fst (roomImage` False zoomed room tsrc)
roomImage _ _ _                   = empty zero zero

roomImage` :: !Bool !Bool !MyRoom !*TagSource -> *(!Image (!MyMap, !MapClick), !*TagSource)
roomImage` mngmnt zoomed room=:{number, exits, roomStatus, actors, inventory} tsrc
  #! (northEs, eastEs, southEs, westEs, upEs, downEs) = foldr foldExit ([], [], [], [], [], []) exits
  #! numNorth       = length northEs
  #! numSouth       = length southEs
  #! numEast        = length eastEs
  #! numWest        = length westEs
  #! widthMul       = toReal (max (max numNorth numSouth) 1)
  #! heightMul      = toReal (max (max (length eastEs) (length westEs)) 1)
  #! multiplier     = if zoomed 2.0 1.0
  #! bgWidth        = multiplier * roomDim * widthMul
  #! bgHeight       = multiplier * roomDim * heightMul
  #! bg             = rect (px bgWidth) (px bgHeight) <@< { fill = toSVGColor "white" }
  #! bg             = bg <@< { onclick = onClick (SelectRoom number), local = False }
  #! statusBadges   = above (repeat AtMiddleX) [] (foldr (mkStatusBadge number mngmnt multiplier) [] roomStatus) Nothing
  #! actorBadges    = above (repeat AtMiddleX) [] (map (scale multiplier multiplier o mkActorBadge) actors) Nothing
  #! numInv         = length inventory
  #! inventoryBadge = if (numInv > 0)
                        (beside (repeat AtMiddleY) [] (map (\i -> scale multiplier multiplier (mkInventoryBadge (toString i % (0,0)))) inventory) Nothing)
                        (empty zero zero)
  #! roomNo         = text myFontDef (toString number) <@< { onclick = onClick (SelectRoom number), local = False }
  #! upDownExits    = above (repeat AtMiddleX) [] (map (scale multiplier multiplier o mkUpDown) (upEs ++ downEs)) Nothing
  #! (topExitAligns, topExitOffsets, topExitImgs) = mkAsOsIs1 bgWidth numNorth (AtLeft, AtTop) northEs
  #! (botExitAligns, botExitOffsets, botExitImgs) = mkAsOsIs1 bgWidth numSouth (AtLeft, AtBottom) southEs
  #! (rExitAligns, rExitOffsets, rExitImgs) = mkAsOsIs2 bgHeight numEast (AtRight, AtTop) eastEs
  #! (lExitAligns, lExitOffsets, lExitImgs) = mkAsOsIs2 bgHeight numWest (AtLeft, AtTop) westEs
  #! total          = overlay ([(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtMiddleY), (AtLeft, AtBottom), (AtRight, AtBottom)] ++ topExitAligns ++ botExitAligns ++ rExitAligns ++ lExitAligns)
                              ([(px 3.0, px 3.0), (px -3.0, px 3.0), (zero, zero), (px 3.0, px -3.0), (px -3.0, px -3.0)] ++ topExitOffsets ++ botExitOffsets ++ rExitOffsets ++ lExitOffsets)
                              ([statusBadges, actorBadges, roomNo, inventoryBadge, upDownExits] ++ topExitImgs ++ botExitImgs ++ rExitImgs ++ lExitImgs) (Just bg)
  = (total, tsrc)
  where
  foldExit :: !(!Exit, !Locked) !(![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)]) -> (![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)])
  foldExit e=:(North _, _) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([e : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit e=:(East _, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [e : eastEs], southEs, westEs, upEs, downEs)
  foldExit e=:(South _, _) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [e : southEs], westEs, upEs, downEs)
  foldExit e=:(West _, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [e : westEs], upEs, downEs)
  foldExit e=:(Up _, _)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [e : upEs], downEs)
  foldExit e=:(Down _, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [e : downEs])

  mkAsOsIs1 :: !Real !Int !(!XAlign, !YAlign) ![(!Exit, !Locked)] -> (![(!XAlign, !YAlign)], ![(!Span, !Span)], [Image (!MyMap, !MapClick)])
  mkAsOsIs1 bgWidth num align es
    #! exitAligns  = repeatn num align
    #! incr        = bgWidth / toReal (num + 1)
    #! exitOffsets = fst (foldr (\_ (xs, n) -> ([(px (n * incr - (exitWidth / 2.0)), px 0.0) : xs], n + 1.0)) ([], 1.0) es)
    #! exitImgs    = map (mkDoor o snd) es
    = (exitAligns, exitOffsets, exitImgs)
    where
    mkDoor :: !Locked -> Image (!MyMap, !MapClick)
    mkDoor locked = xline Nothing (px exitWidth) <@< { stroke = toSVGColor (if locked "black" "white") }
                                                 <@< { strokewidth = px 3.0 }

  mkAsOsIs2 :: !Real !Int !(!XAlign, !YAlign) ![(!Exit, !Locked)] -> (![(!XAlign, !YAlign)], ![(!Span, !Span)], [Image (!MyMap, !MapClick)])
  mkAsOsIs2 bgHeight num align es
    #! exitAligns  = repeatn num align
    #! incr        = bgHeight / toReal (num + 1)
    #! exitOffsets = fst (foldr (\_ (xs, n) -> ([(px 0.0, px (n * incr - (exitWidth / 2.0))) : xs], n + 1.0)) ([], 1.0) es)
    #! exitImgs    = map (mkDoor o snd) es
    = (exitAligns, exitOffsets, exitImgs)
    where
    mkDoor :: !Locked -> Image (!MyMap, !MapClick)
    mkDoor locked = yline Nothing (px exitWidth) <@< { stroke = toSVGColor (if locked "black" "white") }
                                                 <@< { strokewidth = px 3.0 }

onClick :: !MapClick Int !(!MyMap, MapClick) -> (!MyMap, MapClick)
onClick clck _ (m, _) = (m, clck)

mkUpDown :: !(!Exit, Locked) -> Image a
mkUpDown (Up _, _)   = polygon Nothing [(px 0.0, px 0.0), (px 8.0, px -8.0), (px 8.0, px 0.0)]
mkUpDown (Down _, _) = polygon Nothing [(px 0.0, px -8.0), (px 8.0, px 0.0), (px 0.0, px 0.0)]

mkStatusBadge :: Int !Bool !Real !Detector ![Image (!MyMap, !MapClick)] -> [Image (!MyMap, !MapClick)]
mkStatusBadge roomNo mngmnt badgeMult d acc
  #! high = isHigh d
  | high || mngmnt
    #! img = scale badgeMult badgeMult (mkStatusBadgeBackground d) <@< { opacity = if high 1.0 0.3 }
    #! img = if mngmnt
               (img <@< { onclick = onClick (ToggleAlarm roomNo d), local = False })
               img
    = [img : acc]
  | otherwise = acc

mkStatusBadgeBackground :: !Detector -> Image a
mkStatusBadgeBackground (FireDetector  _) = badgeImage <@< { fill = toSVGColor "red"  }
mkStatusBadgeBackground (SmokeDetector _) = badgeImage <@< { fill = toSVGColor "grey" }
mkStatusBadgeBackground (FloodDetector _) = badgeImage <@< { fill = toSVGColor "lightblue" }

mkActorBadge :: !MyActor -> Image a
mkActorBadge {actorStatus = {occupied}, userName, carrying}
  #! actorBadge  = mkActorBadgeBackground occupied
  #! userStr     = toString userName
  #! userInitial = text myFontDef (userStr % (0,0)) <@< { fill = toSVGColor "white" }
  #! actorBadge  = overlay [(AtMiddleX, AtMiddleY)] [] [userInitial] (Just actorBadge)
  #! inventory   = if (length carrying > 0)
                     [mkInventoryBadge (toString (length carrying))]
                     []
  = above (repeat AtMiddleX) [] [actorBadge : inventory] Nothing

mkActorBadgeBackground :: !Availability -> Image a
mkActorBadgeBackground occupied = badgeImage <@< { fill = toSVGColor (case occupied of
                                                                        Available    -> "green"
                                                                        NotAvailable -> "black"
                                                                        Busy         -> "orange")}

mkInventoryBadge :: !String -> Image b
mkInventoryBadge str
  #! txt   = text myFontDef str <@< { fill = toSVGColor "white" }
  = overlay [(AtMiddleX, AtMiddleY)] [] [txt] (Just mkInventoryBadgeBackground)

mkInventoryBadgeBackground :: Image b
mkInventoryBadgeBackground
  = badgeImage <@< { fill = toSVGColor "purple" }

badgeImage :: Image a
badgeImage = rect (px 11.0) (px 11.0) <@< { stroke = toSVGColor "black" }
                                      <@< { strokewidth = px 1.0 }

