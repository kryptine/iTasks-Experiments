module shipAdventure
 
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data

import adventure

:: MyMap		:== MAP 	RoomStatus Object ActorStatus
:: MyActor		:== Actor 	Object ActorStatus
:: MyFloor		:== Floor 	RoomStatus Object ActorStatus
:: MyRoom		:== Room 	RoomStatus Object ActorStatus

:: MapClick     = NoMapClick
                | SelectRoom RoomNumber
                | ToggleAlarm RoomNumber Detector
                | ToggleDoor RoomNumber Exit

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
:: Priority		=	Low | Normal | High | Highest

:: Log			=	{ fromWho 		:: String
					, intendedFor	:: String
					, when			:: DateTime
					, about			:: String
 					}

derive class iTask Detector, Object, ActorStatus, Availability, Priority, Log, MapClick

instance == Object      where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2
instance toString Object where
  toString FireExtinguisher = "Fire extinguiser"
  toString Blanket          = "Blanket"
  toString Plug             = "Plug"

instance toString Exit where toString exit = toSingleLineText exit

instance toString Detector
where toString (FireDetector _)  = "Fire Alarm"
	  toString (SmokeDetector _) = "Smoke Alarm"
	  toString (FloodDetector _) = "Flood Alarm"

isHigh :: !Detector -> Bool
isHigh (FireDetector  b) = b
isHigh (SmokeDetector b) = b
isHigh (FloodDetector b) = b

// should be in the library somewhere

mkTable :: [String]  ![a] -> Table | gText{|*|} a
mkTable	headers a = Table headers (map row a) Nothing
where
	row x =  [Text cell \\ cell <- gText{|*|} AsRow (Just x)]

// shared stores:

myMap  :: Shared MyMap						// map of the ship
myMap = sharedStore "myBuilding" myShip

myLog :: Shared [Log]						// logging events					
myLog = sharedStore "myLog" []

alarmChanged :: Shared Bool					// to notify that one of the alarms has changed, should be turned into a lens
alarmChanged = sharedStore "alarms" False

actorStatusChanged :: Shared Bool			 // to notify that the status of one the actors has changed, should be tuned into a lens 
actorStatusChanged = sharedStore "actors" False

toggle b = not b

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
			,	workflow "alter script" "define your own simulation"								(updateSharedInformation "my script" [] myScript)
		 	]

// initial task to place an actor on the map
// one can only assign tasks to actors on the map

actorWithInstructions :: User  -> Task ()
actorWithInstructions user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addLog user "" ("Entered the building starting in room " <+++ loc)
 	>>|			upd toggle actorStatusChanged 
 	>>|			addActorToMap mkRoom (newActor user) loc myMap
where
	newActor user 			
		= {userName = user, carrying = [], actorStatus = {occupied = Available}}
 
// given the alarms one has to decide which tasks to assign to handle the situation

giveInstructions :: Task ()
giveInstructions 
=	forever 
	(					get currentUser
	>>= \me ->  		showAlarms 
	>>= \alarms -> 		(							selectAlarm alarms
							>&> 					withSelection (viewInformation () [] "No Alarm Selected")
				 			\(alarmLoc,detector) -> selectSomeOneToHandle (alarmLoc,detector)
				 			>&>						withSelection (viewInformation () [] "No Crew Member Selected")
				 			\(actorLoc,actor) ->	viewRelativeStatus (actorLoc,actor) (alarmLoc,detector)
				 			>&>						withSelection (viewInformation () [] "")
							\_ ->					updateChoice "Select the Priority : " [ChooseWith (ChooseFromRadioButtons id)] [Low, Normal, High, Highest] High
							>>* 					[ OnAction ActionByHand  	(hasValue (\prio -> handleAlarm (me,(alarmLoc,detector),(actorLoc,actor),prio)))
	       											, OnAction ActionSimulated  (hasValue (\prio -> autoHandleAlarm me actor.userName (alarmLoc,detector) @! ()))
	       											, OnAction ActionScript  (hasValue (\prio -> autoHandleWithScript (me,(alarmLoc,detector),(actorLoc,actor),prio) @! ()))
													, OnAction ActionCancel (always (return ()))
	       											]
	       				)
	)
where
	ActionByHand 		= Action "By Hand"  []
	ActionSimulated 	= Action "Simulate" []	
	ActionScript 		= Action "Simulate with Script" []	

	selectAlarm alarms
		=	whileUnchanged alarmChanged 
				(\_ -> enterChoice "Choose which Alarm to handle : " [ChooseWith (ChooseFromRadioButtons showAlarm)] alarms)

	selectSomeOneToHandle :: (RoomNumber,Detector) -> Task (Int,MyActor)
	selectSomeOneToHandle (number,detector)
		=	whileUnchanged actorStatusChanged 
				 (\_ ->		  		get myMap
				  >>= \curMap ->  	let allActors = [(room,actor) \\ (room,actor) <- findAllActors curMap | actor.actorStatus.occupied === Available ] in
									enterChoice (if (isEmpty allActors) "No one available at all !" ("Who should handle: " <+++ showAlarm (number,detector))) [] allActors)

	viewRelativeStatus :: (RoomNumber,MyActor) (RoomNumber,Detector) -> Task ()
	viewRelativeStatus (actorLoc,actor) (alarmLoc,FireDetector _)
		= whileUnchanged myMap 
				\curMap -> 	let		(nrExt,(extLoc,distExt,_)) 					= pathToClosestObject FireExtinguisher actorLoc curMap
									(nrBlankets,(blanketLoc,distBlankets,_)) 	= pathToClosestObject Blanket 			actorLoc curMap
							in	viewInformation "" [] 
									(mkTable [ "Object Description", "Rooms Away from " <+++ actor.userName <+++ " in Room " <+++ actorLoc]
											 [ ("The Fire Detected in Room " <+++ alarmLoc, toString (length (shipShortestPath actorLoc alarmLoc curMap)))
											 , ("Closest Extinquisher (" <+++ nrExt <+++ " left in total)", toString distExt <+++ " (in Room " <+++ extLoc <+++ " )")
											 , ("Closest Blanket ("	<+++ nrBlankets	<+++ " left in total)", toString distBlankets <+++ " (in Room " <+++ blanketLoc <+++ " )")
											 ]) @! ()
	viewRelativeStatus (actorLoc,actor) (alarmLoc,SmokeDetector _) 
		= whileUnchanged myMap 
				\curMap ->	viewInformation "" [] 
									(mkTable [ "Object Description", "Rooms Away from " <+++ actor.userName <+++ " in Room " <+++ actorLoc]
											 [ ("The Smoke Detected in Room " <+++ alarmLoc, length (shipShortestPath actorLoc alarmLoc curMap))
											 ]) @! ()
	viewRelativeStatus (actorLoc,actor) (alarmLoc,FloodDetector _)  
		= whileUnchanged myMap 
				\curMap -> 	let	(nrPlugs,(plugLoc,distPlugs,_)) 		= pathToClosestObject Plug actorLoc curMap
							in	viewInformation "" [] 
									(mkTable [ "Object Description", "Rooms Away from " <+++ actor.userName <+++ " in Room " <+++ actorLoc]
											 [ ("The Flood Detected in Room " <+++ alarmLoc, toString (length (shipShortestPath actorLoc alarmLoc curMap)))
											 , ("Closest plug (" <+++ nrPlugs <+++ " left in total)", toString distPlugs <+++ " (in Room " <+++ plugLoc <+++ " )")
											 ]) @! ()

handleAlarm (me,(alarmLoc,detector),(actorLoc,actor),priority)
= 		updStatusOfActor actor.userName Busy myMap
 >>|	addLog ("Commander " <+++ me) actor.userName (message "Start Handling ")
 >>| 	appendTopLevelTaskPrioFor me (message "Wait for ") "High" True 
 			(handleWhileWalking actor (message "Handle ") (toSingleLineText priority) 
 			 (taskToDo (alarmLoc,detector)) ) @! ()
where
	message work = (work <+++ toString detector <+++ " in Room " <+++ alarmLoc)

	handleWhileWalking :: MyActor String String (MyActor MyRoom MyMap -> Task (Maybe (Task (Maybe String)))) -> Task () 
	handleWhileWalking actor title priority task 
		=					(((actor.userName,title)  @:( 				moveAround mkRoom actor (Just task) myMap 
							 							>>= \mbTask ->  fromJust mbTask))

							-||-
							(viewInformation ("Cancel task \"" <+++ title <+++ "\"") [] () @! Nothing))
		>>= \mba ->	if (isNothing mba)
		 					(viewInformation ("Task " <+++ title <+++ " has been cancelled by you") [] ())
							(viewInformation ("Task " <+++ title <+++ " terminated normally, returning:") [] (fromJust mba) @! ())
		>>|			return ()
	

	taskToDo :: (RoomNumber,Detector) MyActor MyRoom MyMap -> Task (Maybe (Task (Maybe String)))
	taskToDo (alarmLoc,detector) curActor curRoom curMap
		=		viewInformation ("Handle " <+++ toString detector <+++ " in Room: " <+++ alarmLoc) []  ()
				-|| 
				(let	path												= shipShortestPath curRoom.number alarmLoc curMap
						alarmDist											= length path			 
						(nrExt,(extLoc,distExt,dirExt)) 					= pathToClosestObject FireExtinguisher curRoom.number curMap
						(nrBlankets,(blanketLoc,distBlankets,dirBlanket)) 	= pathToClosestObject Blanket curRoom.number curMap
						(nrPlugs,(plugLoc,distPlugs,dirPlug)) 				= pathToClosestObject Plug curRoom.number curMap
				in viewInformation "" []
						(mkTable  [ "Object Description", "Rooms Away from " <+++ curActor.userName]
								 [ (toString detector <+++ " Detected", toString alarmDist <+++ " (in Room " <+++ alarmLoc <+++ goto path <+++ ")")
								 , ("Closest Extinquisher (" <+++ nrExt <+++ " left in total)", toString distExt <+++ " (in Room " <+++ extLoc <+++ goto dirExt <+++ "}")
								 , ("Closest Blanket ("	<+++ nrBlankets	<+++ " left in total)", toString distBlankets <+++ " (in Room " <+++ blanketLoc <+++ goto dirBlanket <+++ ")")
								 , ("Closest plug (" <+++ nrPlugs <+++ " left in total)", toString distPlugs <+++ " (in Room " <+++ plugLoc <+++ goto dirPlug <+++ ")")
								 ])) @! ()
				>>* [OnAction (Action "Use Fire Extinguisher" []) 	(ifCond (mayUseExtinguisher detector) (return (Just useExtinquisher)))
					,OnAction (Action "Use Blanket" []) 			(ifCond (mayUseBlanket detector) (return (Just useBlanket)))
					,OnAction (Action "Use Plug" []) 				(ifCond (mayUsePlug detector) (return (Just usePlug)))
					,OnAction (Action "Smoke Investigated" []) 		(ifCond (mayDetectedSmoke detector) (return (Just smokeReport)))

 					,OnAction (Action "I give up" []) (always (return (Just giveUp)))
					]
	where 
		mayUseExtinguisher (FireDetector True) 	= curRoom.number == alarmLoc && (isMember FireExtinguisher curActor.carrying)
		mayUseExtinguisher _ 					= False

		mayUseBlanket (FireDetector True) 		= curRoom.number == alarmLoc && (isMember Blanket curActor.carrying)
		mayUseBlanket _ 						= False

		mayUsePlug  (FloodDetector True) 		= curRoom.number == alarmLoc && (isMember Plug curActor.carrying)
		mayUsePlug _							= False

		mayDetectedSmoke (SmokeDetector True) 	= curRoom.number == alarmLoc
		mayDetectedSmoke _						= False

		useExtinquisher		=		useObject alarmLoc FireExtinguisher curActor myMap
   								>>|	resetAlarm (alarmLoc,detector) actor myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Fire Extinguished !" [] ()
   								>>| return (Just "Fire Extinguised")

		useBlanket			=		useObject alarmLoc Blanket curActor myMap
   								>>|	resetAlarm (alarmLoc,detector) actor myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Fire Extinguished !" [] ()
   								>>| return (Just "Fire Extinguised")
		
		usePlug				=		useObject alarmLoc Plug curActor myMap
   								>>|	resetAlarm (alarmLoc,detector) actor myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Flooding Stopped !" [] ()
   								>>| return (Just "Flooding Stopped")

		smokeReport			=		resetAlarm (alarmLoc,detector) actor myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Reason of Smoke Detected !" [] ()
   								>>| return (Just "Don't smoke under a smoke detector !!")

		giveUp 				=		updStatusOfActor curActor.userName Available myMap
								>>|	return (Just "I gave up, send somebody else...")

		goto []  = ", you are there"
		goto dir = ", goto " +++ toString (hd dir)


updStatusOfActor :: User Availability (Shared MyMap) -> Task ()
updStatusOfActor user availability smap
	=		updActorStatus user (\st -> {st & occupied = availability}) smap
 	>>|		upd toggle actorStatusChanged
 	>>|		addLog user "" ("Has become " <+++ availability)

resetAlarm :: (RoomNumber,Detector) MyActor (Shared MyMap) -> Task ()
resetAlarm (alarmLoc,detector) actor smap
	= 		updRoomStatus alarmLoc (updDetector resetDetector detector) smap
	>>|		upd toggle alarmChanged		
	>>|		addLog actor.userName alarmLoc  ("Resets " <+++ detector <+++ " to False.") 

// simulate via auto stuf

autoHandleAlarm commander user (alarmLoc,detector) 
	=	appendTopLevelTaskPrioFor user ("Auto handling " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True 
 			(startSimulation commander user (alarmLoc,detector)) @! ()

startSimulation :: User User (RoomNumber,Detector) -> Task Bool
startSimulation commander user (alarmLoc,detector) 
	=				updStatusOfActor user Busy myMap 
 	>>|				addLog ("Commander " <+++ commander) user ("Simulate Handling " <+++ toString detector <+++ " detected in " <+++ alarmLoc)
 	>>|				get myMap 
	>>= \curMap ->	let (myLoc,curActor) 		= fromJust (findUser user curMap) 						
						(mbObjectLoc,mbObject)  = findClosestObject myLoc (alarmLoc,detector) curMap		
					in if (isNothing mbObjectLoc)
						  (return False)
						  if (isNothing mbObject)													
						  	(simulateHandling myLoc alarmLoc detector curActor myMap)
						  	(simulateHandlingWithObject myLoc (fromJust mbObject) (fromJust mbObjectLoc) alarmLoc detector curActor myMap) 
	>>= \succes ->	updStatusOfActor user Available myMap 
 	>>|				addLog user commander  ("Simulation Handling " <+++ toString detector <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
 	>>|				return True

simulateHandling startLoc alarmLoc detector actor smap
	=						autoMove startLoc alarmLoc shipShortestPath actor smap
   	>>= \targetReached ->	if targetReached (resetAlarm (alarmLoc,detector) actor smap @! True)
   							(return False)

simulateHandlingWithObject startLoc object objectLoc alarmLoc detector actor smap
	=						autoMove startLoc objectLoc shipShortestPath actor smap
   	>>= \objectReached	->	if objectReached	(pickupObject objectLoc object actor smap
   	>>= \objectFound	->	if objectFound		(autoMove objectLoc alarmLoc shipShortestPath actor smap
   	>>= \targetReached ->	if targetReached	(useObject alarmLoc object actor smap
   	>>= \used ->			if used		 		(resetAlarm (alarmLoc,detector) actor smap @! True)
												(return False))
												(return False))
												(return False))
												(return False)

findClosestObject :: RoomNumber (RoomNumber,Detector) MyMap -> (Maybe RoomNumber,Maybe Object)
findClosestObject  myLoc (alarmLoc,detector) curMap
	= 	case detector of
			(SmokeDetector _) = (Just myLoc,Nothing)
			(FloodDetector _) = case findClosest myLoc Plug curMap of
								 Nothing	-> (Nothing,Nothing)
								 objLoc	 	-> (objLoc,Just Plug)
			(FireDetector _)  = case (findClosest myLoc Blanket curMap, findClosest myLoc FireExtinguisher curMap) of
								 (Nothing,Nothing)  -> (Nothing,Nothing)
								 (Nothing,objLoc)   -> (objLoc,Just FireExtinguisher)
								 (objLoc,Nothing)   -> (objLoc,Just Blanket)
								 (objLoc1,objLoc2)  ->  if (fromJust objLoc1 < fromJust objLoc2)
								 							(objLoc1,Just Blanket) (objLoc2,Just FireExtinguisher)

findClosest roomNumber object curMap
	= 	let revPath = reverse (thd3 (snd (pathToClosestObject object roomNumber curMap)))
		in if (isEmpty revPath) Nothing (Just (fromExit (hd revPath)))
		
pathToClosestObject :: Object RoomNumber MyMap -> (Int,(RoomNumber,Int,[Exit]))  // returns: number of objects found, location of object, distance to object, shortest path to obejct
pathToClosestObject kind actorLoc curMap
	= (numberResources, if (numberResources == 0) (-1,-1,[]) (hd spath))
	where
		numberResources = length spath 
		spath = sortBy (\(i,l1,p1) (j,l2,p2) -> i < j)   [let path = shipShortestPath actorLoc objectLoc curMap in (objectLoc, length path, path) 
													\\ (objectLoc,found) <- findAllObjects curMap | found == kind ]


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

// scripted simulation

:: Target		= 	Room Int
				|	Nearest Object
				|	TargetRoom
:: Script		=	MoveTo Target
				|	Take Object
				|	Drop Object
				|	Use Object
				|	ReSetTargetDetector 
				|	If Condition Script Script
:: Condition	=	InRoom MyRoom
				|	ObjectInRoom Object
				|	CarriesObject Object
				|	ExistObject Object
				|	And Condition Condition
				|	Or Condition Condition

derive class iTask Target, Script, Condition

myScript :: Shared [Script]
myScript = sharedStore "myScript" []

autoHandleWithScript :: (User,(RoomNumber,Detector),(RoomNumber,MyActor),Priority) -> Task ()
autoHandleWithScript  (commander,(alarmLoc,detector),(actorLoc,actor),prio)
	= 				get myScript 
	>>= \script ->	appendTopLevelTaskPrioFor actor.userName ("Auto script " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True 
 					(		updStatusOfActor actor.userName  Busy myMap 
 					 >>|	perform script (actorLoc,actor)
 					 >>|	updStatusOfActor actor.userName  Available myMap 
 					 ) @! ()

where
	perform :: [Script] (RoomNumber,MyActor) -> Task Bool
	perform [] (actorLoc,actor)								
		=	return True	
	perform [MoveTo target:next] (actorLoc,actor)	
		=					get myMap
		>>= \curMap ->		return (whereIs target actorLoc curMap)
		>>= \newLoc ->		autoMove actorLoc newLoc shipShortestPath actor myMap
		>>| 				perform next (newLoc,actor) 
	perform [Take object:next] (actorLoc,actor)	
		=					pickupObject actorLoc object actor myMap
		>>|					perform next (actorLoc,actor)
	perform [Drop object:next] (actorLoc,actor)	
		=					dropDownObject actorLoc object actor myMap
		>>|					perform next (actorLoc,actor)
	perform [Use object:next] (actorLoc,actor)	
		=					useObject actorLoc object actor myMap
		>>|					perform next (actorLoc,actor)
	perform [ReSetTargetDetector:next] (actorLoc,actor)	
		=					resetAlarm (alarmLoc,detector) actor myMap
		>>|					perform next (actorLoc,actor)

	whereIs (Room nr) actorLoc curMap			= nr
	whereIs (Nearest object) actorLoc curMap	= let (_,(objectLoc,_,_)) = pathToClosestObject object actorLoc curMap in objectLoc
	whereIs TargetRoom actorLoc curMap		= alarmLoc

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
                             Value (ToggleAlarm selRoom d)   _ -> Just (updRoomStatus selRoom (updDetector toggleDetector d) myMap >>| setRoomDetectors)
                             Value (ToggleDoor selRoom exit) _ -> Just (toggleExit selRoom exit myMap >>| setRoomDetectors)
                             _ -> Nothing
                   )]

updDetector :: !(Detector -> Detector) !Detector !RoomStatus -> RoomStatus
updDetector f d r = [if (d =+?= d`) (f d`) d` \\ d` <- r]

toggleDetector :: !Detector -> Detector
toggleDetector (FireDetector  b) = FireDetector  (not b)
toggleDetector (SmokeDetector b) = SmokeDetector (not b)
toggleDetector (FloodDetector b) = FloodDetector (not b)

setDetector :: !Detector -> Detector
setDetector (FireDetector  b) = FireDetector  True
setDetector (SmokeDetector b) = SmokeDetector True
setDetector (FloodDetector b) = FloodDetector True

resetDetector :: !Detector -> Detector
resetDetector (FireDetector  b) = FireDetector  False
resetDetector (SmokeDetector b) = SmokeDetector False
resetDetector (FloodDetector b) = FloodDetector False

// Logging events

addLog :: a b c -> Task () | toString a & toString b & toString c
addLog who location about
	=				 get currentDateTime
	>>= \dateTime -> upd (\log -> [{ fromWho = (toString who), when = dateTime, intendedFor = toString location, about = toString about}:log]) myLog
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
                  ]
	back0		= {name = "back 0",    number = 1,  roomStatus = detectors, inventory = [], exits = [(South 2, False), (South 3, False), (South 4, False)], actors = []}
	room01		= {name = "room 0.1",   number = 2,  roomStatus = detectors, inventory = [], exits = [(North 1, False), (South 5, False), (Down 13, False)], actors = []}			
	room02		= {name = "room 0.2",   number = 3,  roomStatus = detectors, inventory = [], exits = [(North 1, False), (South 5, False)], actors = []}			
	room03		= {name = "room 0.3",   number = 4,  roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 1, False), (South 5, False)], actors = []}
	corridor0	= {name = "corridor 0", number = 5,  roomStatus = detectors, inventory = [], exits = [(North 2, False), (North 3, False), (North 4, False)
                                                                                                     , (South 6, False), (South 7, False), (South 8, False)
                                                                                                     ], actors = []}
	room04		= {name = "room 0.4",   number = 6,  roomStatus = detectors, inventory = [], exits = [(North 5, False), (South 9, False)], actors = []}			
	room05		= {name = "room 0.5",   number = 7,  roomStatus = detectors, inventory = [Blanket], exits = [(North 5, False), (South 9, False)], actors = []}			
	room06		= {name = "room 0.6",   number = 8,  roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 5, False), (South 10, False)], actors = []}
	room07		= {name = "room 0.7",   number = 9,  roomStatus = detectors, inventory = [Blanket], exits = [(North 6, False), (North 7, False)], actors = []}			
	room08		= {name = "room 0.8",   number = 10, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 8, False), (Down 21, False)], actors = []}

	floor1  	= [ [back1]
                  , [room11, room12, room13]
                  , [corridor1]
                  , [room14, room15, room16]
                  , [room17, room18]
                  ]
	back1		= {name = "back 1",    number = 12, roomStatus = detectors, inventory = [], exits = [(South 13, False), (South 14, False), (South 15, False)], actors = []}
	room11		= {name = "room 1.1",   number = 13, roomStatus = detectors, inventory = [], exits = [(North 12, False), (South 16, False), (Up 2, False)], actors = []}
	room12		= {name = "room 1.2",   number = 14, roomStatus = detectors, inventory = [], exits = [(North 12, False), (South 16, False), (East 15, False)], actors = []}
	room13		= {name = "room 1.3",   number = 15, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 12, False), (South 16, False), (West 14, False)], actors = []}
	corridor1	= {name = "corridor 1", number = 16, roomStatus = detectors, inventory = [], exits = [(North 13, False), (North 14, False), (North 15, False)
                                                                                                     , (South 17, False), (South 18, False), (South 19, False)
                                                                                                     ], actors = []}
	room14		= {name = "room 1.4",   number = 17, roomStatus = detectors, inventory = [], exits = [(North 16, False), (South 20, False)], actors = []}
	room15		= {name = "room 1.5",   number = 18, roomStatus = detectors, inventory = [Blanket,Plug], exits = [(North 16, False), (South 20, False)], actors = []}
	room16		= {name = "room 1.6",   number = 19, roomStatus = detectors, inventory = [], exits = [(North 16, False), (South 21, False)], actors = []}
	room17		= {name = "room 1.7",   number = 20, roomStatus = detectors, inventory = [Blanket,Plug], exits = [(North 17, False), (North 18, False)], actors = []}
	room18		= {name = "room 1.8",   number = 21, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 19, False), (Up 10, False)], actors = []}
	
	detectors = [FireDetector False,SmokeDetector False,FloodDetector False]


// shortest path given the alarms set on the ship

shipShortestPath :: RoomNumber RoomNumber MyMap -> [Exit]
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

floorWidth :: !MyFloor -> Int
floorWidth floor = foldr (\xs -> max (length xs)) 0 floor

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
exitWidth =: 16.0

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
  #! upDownExits    = above (repeat AtMiddleX) [] (map (\x=:(e, _) -> scale multiplier multiplier (mkUpDown x) <@< { onclick = onClick (ToggleDoor number e), local = False }) (upEs ++ downEs)) Nothing
  #! (topExitAligns, topExitOffsets, topExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numNorth (AtLeft, AtTop)    northEs
  #! (botExitAligns, botExitOffsets, botExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numSouth (AtLeft, AtBottom) southEs
  #! (rExitAligns,   rExitOffsets,   rExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numEast  (AtRight, AtTop)   eastEs
  #! (lExitAligns,   lExitOffsets,   lExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numWest  (AtLeft, AtTop)    westEs
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

  mkAsOsIs :: !Real !(Span -> (!Span, !Span)) !(Image (!MyMap, !MapClick)) !Real !Int !(!XAlign, !YAlign) ![(!Exit, !Locked)]
            -> (![(!XAlign, !YAlign)], ![(!Span, !Span)], [Image (!MyMap, !MapClick)])
  mkAsOsIs multiplier mkTuple doorImg bgSize num align es
    #! exitAligns  = repeatn num align
    #! exitOffsets = reverse (fst (foldr (\_ (xs, n) -> ([mkTuple (px n) : xs], n + roomDim * multiplier)) ([], (roomDim * multiplier - exitWidth) / 2.0) es))
    #! exitImgs    = map mkDoor es
    = (exitAligns, exitOffsets, exitImgs)
    where
    mkDoor :: !(!Exit, !Locked) -> Image (!MyMap, !MapClick)
    mkDoor (exit, locked)
      = doorImg
          <@< { stroke = toSVGColor "black" }
          <@< { strokewidth = px 1.0 }
          <@< { fill = toSVGColor (if locked "black" "white") }
          <@< { onclick = onClick (ToggleDoor number exit), local = False }

onClick :: !MapClick Int !(!MyMap, MapClick) -> (!MyMap, MapClick)
onClick clck _ (m, _) = (m, clck)
import StdDebug
mkUpDown :: !(!Exit, !Locked) -> Image (!MyMap, !MapClick)
mkUpDown (e=:(Up _), l)
  = polygon Nothing [(px 0.0, px 0.0), (px 12.0, px -12.0), (px 12.0, px 0.0)]
      <@< { opacity = if l 0.3 1.0 }
mkUpDown (e=:(Down _), l)
  = polygon Nothing [(px 0.0, px -12.0), (px 12.0, px 0.0), (px 0.0, px 0.0)]
      <@< { opacity = if l 0.3 1.0 }

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
  #! inventory   = map (\i -> mkInventoryBadge (toString i % (0,0))) carrying
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
