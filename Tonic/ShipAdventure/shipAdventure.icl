implementation module shipAdventure

import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data

import shipType, logging, scripting

// the next function should be placed in the library somewhere

mkTable :: [String]  ![a] -> Table | gText{|*|} a
mkTable	headers a = Table headers (map row a) Nothing
where
	row x =  [Text cell \\ cell <- gText{|*|} AsRow (Just x)]

myTasks :: [Workflow]
myTasks = 	[	workflow "walk around"	"enter map, walk around, follow instructions of commander"  currentUserWalkAround
			,	workflow "commander"	"give instructions to crew members on the map" 				giveInstructions
			,	workflow "alter script" "define your own simulation"								mkScript
		 	]

currentUserWalkAround :: Task ()
currentUserWalkAround = get currentUser >>= actorWithInstructions

// initial task to place an actor on the map
// one can only assign tasks to actors on the map

actorWithInstructions :: User  -> Task ()
actorWithInstructions user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addLog user "" ("Entered the building starting in room " <+++ loc)
 	>>|			signalActorStatusChange 
 	>>|			addActorToMap mkRoom (newActor user) loc myMap
where
	newActor user 			
		= {userName = user, carrying = [], actorStatus = {occupied = Available}}
 
// given the alarms one has to decide which tasks to assign to handle the situation

giveInstructions :: Task ()
giveInstructions =
  forever
  (          get currentUser
  >>= \me -> (                        enterChoiceWithShared "Choose which Alarm to handle : " [ChooseWith (ChooseFromRadioButtons showAlarm)] allActiveAlarms
             >&>                      withSelection (viewInformation () [] "No Alarm Selected")
             \(alarmLoc, detector) -> selectSomeOneToHandle (alarmLoc, detector)
             >&>                      withSelection (viewInformation () [] "No Crew Member Selected")
             \(actorLoc, actor) ->    viewRelativeStatus (actorLoc, actor) (alarmLoc, detector)
             >&>                      withSelection (viewInformation () [] "No status")
             \_ ->                    updateChoice "Select the Priority : " [ChooseWith (ChooseFromRadioButtons id)] [Low, Normal, High, Highest] High
             >>* [ OnAction ActionByHand    (hasValue (\prio -> handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), prio)))
                 , OnAction ActionSimulated (hasValue (\prio -> autoHandleAlarm me actor.userName (alarmLoc, detector) @! ()))
                 , OnAction ActionScript    (hasValue (\prio -> autoHandleWithScript (me, (alarmLoc, detector), (actorLoc, actor), prio) @! ()))
                 , OnAction ActionCancel    (always (return ()))
                 ]
            )
  )
where
    ActionByHand    = Action "By Hand"  []
    ActionSimulated = Action "Simulate" []
    ActionScript    = Action "Simulate with Script" []

    showAlarm (alarmLoc, detector) = "Room : " <+++ alarmLoc <+++ " : " <+++ toString detector <+++ "!"

    selectSomeOneToHandle :: (RoomNumber, Detector) -> Task (Int, MyActor)
    selectSomeOneToHandle (number,detector)
      = enterChoiceWithShared ("Who should handle: " <+++ showAlarm (number, detector)) [] allAvailableActors

    viewRelativeStatus :: (RoomNumber,MyActor) (RoomNumber,Detector) -> Task ()
    viewRelativeStatus (actorLoc, actor) (alarmLoc, FireDetector _)
        = viewSharedInformation () [ViewWith mkView] myMap @! ()
        where
        mkView curMap
          # (nrExt, (extLoc, distExt, _))               = pathToClosestObject FireExtinguisher actorLoc curMap
          # (nrBlankets, (blanketLoc, distBlankets, _)) = pathToClosestObject Blanket          actorLoc curMap
          = mkTable [ "Object Description", "Rooms Away from " <+++ actor.userName <+++ " in Room " <+++ actorLoc]
                    [ ("The Fire Detected in Room " <+++ alarmLoc, toString (length (shipShortestPath actorLoc alarmLoc curMap)))
                    , ("Closest Extinquisher (" <+++ nrExt <+++ " left in total)", toString distExt <+++ " (in Room " <+++ extLoc <+++ " )")
                    , ("Closest Blanket ("	<+++ nrBlankets	<+++ " left in total)", toString distBlankets <+++ " (in Room " <+++ blanketLoc <+++ " )")
                    ]
    viewRelativeStatus (actorLoc, actor) (alarmLoc, SmokeDetector _)
        = viewSharedInformation () [ViewWith mkView] myMap @! ()
        where
        mkView curMap
          = mkTable [ "Object Description", "Rooms Away from " <+++ actor.userName <+++ " in Room " <+++ actorLoc]
                    [ ("The Smoke Detected in Room " <+++ alarmLoc, length (shipShortestPath actorLoc alarmLoc curMap))
                    ]
    viewRelativeStatus (actorLoc, actor) (alarmLoc, FloodDetector _)
        = viewSharedInformation () [ViewWith mkView] myMap @! ()
        where
        mkView curMap
          # (nrPlugs, (plugLoc, distPlugs, _)) = pathToClosestObject Plug actorLoc curMap
          = mkTable [ "Object Description", "Rooms Away from " <+++ actor.userName <+++ " in Room " <+++ actorLoc]
                    [ ("The Flood Detected in Room " <+++ alarmLoc, toString (length (shipShortestPath actorLoc alarmLoc curMap)))
                    , ("Closest plug (" <+++ nrPlugs <+++ " left in total)", toString distPlugs <+++ " (in Room " <+++ plugLoc <+++ " )")
                    ]

handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), priority)
  =   updStatusOfActor actor.userName Busy myMap
  >>| addLog ("Commander " <+++ me) actor.userName (message "Start Handling ")
  >>| appendTopLevelTaskPrioFor me (message "Wait for ") "High" True
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
   								>>|	setAlarm actor.userName (alarmLoc,detector) False myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Fire Extinguished !" [] ()
   								>>| return (Just "Fire Extinguised")

		useBlanket			=		useObject alarmLoc Blanket curActor myMap
   								>>|	setAlarm actor.userName (alarmLoc,detector) False myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Fire Extinguished !" [] ()
   								>>| return (Just "Fire Extinguised")
		
		usePlug				=		useObject alarmLoc Plug curActor myMap
   								>>|	setAlarm actor.userName (alarmLoc,detector) False myMap
   								>>| updStatusOfActor curActor.userName Available myMap
   								>>| viewInformation "Well Done, Flooding Stopped !" [] ()
   								>>| return (Just "Flooding Stopped")

		smokeReport			=		setAlarm actor.userName (alarmLoc,detector) False myMap
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
 	>>|		signalActorStatusChange
 	>>|		addLog user "" ("Has become " <+++ availability)


autoHandleWithScript :: (User,(RoomNumber,Detector),(RoomNumber,MyActor),Priority) -> Task ()
autoHandleWithScript  (commander,(alarmLoc,detector),(actorLoc,actor),prio)
	= 				get myScript 
	>>= \script ->	appendTopLevelTaskPrioFor actor.userName ("Auto script " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True 
 					(		updStatusOfActor actor.userName  Busy myMap 
 						>>|	addLog ("Commander " <+++ commander) actor.userName ("Simulate Handling " <+++ toString detector <+++ " detected in " <+++ alarmLoc)
 						>>|	interperScript (alarmLoc,detector) actor.userName script // perform script (actorLoc,actor)
 					 	>>|	updStatusOfActor actor.userName  Available myMap 
 						>>| addLog actor.userName commander  ("Simulation Handling " <+++ toString detector <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
 					 ) @! ()

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
   	>>= \targetReached ->	if targetReached (setAlarm actor.userName (alarmLoc,detector) False smap @! True)
   							(return False)

simulateHandlingWithObject startLoc object objectLoc alarmLoc detector actor smap
	=						autoMove startLoc objectLoc shipShortestPath actor smap
   	>>= \objectReached	->	if objectReached	(pickupObject objectLoc object actor smap
   	>>= \objectFound	->	if objectFound		(autoMove objectLoc alarmLoc shipShortestPath actor smap
   	>>= \targetReached ->	if targetReached	(useObject alarmLoc object actor smap
   	>>= \used ->			if used		 		(setAlarm actor.userName (alarmLoc,detector) False smap @! True)
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
mkRoom room = viewInformation "Room Status" [imageView (\(room, _) -> roomImage True (Just room)) (\_ _ -> Nothing)] (room, NoMapClick) @! ()

