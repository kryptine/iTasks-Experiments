implementation module ShipAdventure.Core

import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data

import ShipAdventure.Types, Adventure.Logging, ShipAdventure.Scripting
import ShipAdventure.PathFinding

// the next function should be placed in the library somewhere

mkTable :: [String]  ![a] -> Table | gText{|*|} a
mkTable	headers a = Table headers (map row a) Nothing
  where
  row x = [Text cell \\ cell <- gText{|*|} AsRow (Just x)]

myTasks :: [Workflow]
myTasks = [ workflow "walk around"  "enter map, walk around, follow instructions of commander" currentUserWalkAround
          , workflow "commander"    "give instructions to crew members on the map"             giveInstructions
          , workflow "alter fire script" "define your own script for handling fires"           changeFireScript
          , workflow "alter flood script" "define your own script for handling floods"          changeFloodScript
          , workflow "alter smoke script" "define your own script for handling smoke"           changeSmokeScript
          ]

currentUserWalkAround :: Task ()
currentUserWalkAround = get currentUser >>= actorWithInstructions

// initial task to place an actor on the map
// one can only assign tasks to actors on the map

actorWithInstructions :: User -> Task ()
actorWithInstructions user
  # actor = newActor user
  =           get myMap
  >>= \map -> case findUser actor.userName map of
                Nothing
                  =           enterInformation "Which room do you want to start in?" []
                  >>= \loc -> addLog user "" ("Entered the building starting in room " <+++ loc)
                  >>|         addActorToMap mkRoom actor loc myMap
                Just (_, me) = moveAround mkRoom me noTask myMap @! ()
  where
  newActor user
    = {userName = user, carrying = [], actorStatus = {occupied = Available}}

  noTask :: Maybe (ActorTask r o a ()) | iTask r & iTask o & iTask a & Eq o
  noTask = Nothing

// given the alarms one has to decide which tasks to assign to handle the situation

spToDistString :: (Maybe [Exit]) -> String
spToDistString (Just es) = toString (length es)
spToDistString _                 = "Room unreachable!"

spToDistString2 :: (Maybe ([Exit],Distance)) -> String
spToDistString2 (Just (es,_)) = toString (length es)
spToDistString2 _                 = "Room unreachable!"

roomToString :: Int -> String
roomToString n
| n < 0 = "Room unreachable!"
= toString n


giveInstructions :: Task ()
giveInstructions =
  forever
  (          get currentUser
  >>= \me -> (                        enterChoiceWithShared "Choose which Alarm to handle : " [ChooseWith (ChooseFromRadioButtons showAlarm)] allActiveAlarms
             >&>                      withSelection (viewInformation () [] "No Alarm Selected")
             \(alarmLoc, detector) -> selectSomeOneToHandle (alarmLoc, detector)
             >&>                      withSelection (viewInformation () [] "No Crew Member Selected")
             \(actorLoc, actor) ->    viewRelativeStatus (actorLoc, actor) (alarmLoc, detector)
                                      ||-
                                      updateChoice "Select the Priority : " [ChooseWith (ChooseFromRadioButtons id)] [Low, Normal, High, Highest] High
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

  showAlarm (alarmLoc, detector) = "Room " <+++ alarmLoc <+++ " : " <+++ toString detector <+++ "!"

  selectSomeOneToHandle :: (RoomNumber, Detector) -> Task (Int, MyActor)
  selectSomeOneToHandle (number, detector)
    = enterChoiceWithShared ("Who should handle: " <+++ showAlarm (number, detector))
    	[ChooseWith (ChooseFromGrid (\(roomNumber,actor) -> (roomNumber, actor.userName, actor.actorStatus)))] allAvailableActors

  viewRelativeStatus :: (RoomNumber,MyActor) (RoomNumber,Detector) -> Task ()
  viewRelativeStatus (actorLoc, actor) (alarmLoc, FireDetector _)
      = viewSharedInformation () [ViewWith mkView] myMap @! ()
      where
      mkView curMap
        # (_,eCost,nrExt, (extLoc, distExt, _))               = smartShipPathToClosestObject FireExtinguisher actorLoc alarmLoc curMap
        # (_,bCost,nrBlankets, (blanketLoc, distBlankets, _)) = smartShipPathToClosestObject Blanket          actorLoc alarmLoc curMap
        # fireDist                                            = spToDistString2 (shipShortestPath actorLoc alarmLoc curMap)
        = mkTable [ "Object Description",                                   "Located in Room" ,      "Distance from " <+++ actor.userName, "Route length"]
                  [ ("Fire Alarm !! " ,                                     roomToString alarmLoc,   fireDist,                             fireDist)
                  , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)", roomToString extLoc,     roomToString distExt,                 toString eCost)
                  , ("Closest Blanket (" <+++ nrBlankets <+++ " in reach)", roomToString blanketLoc, roomToString distBlankets,            toString bCost)
                  ]
  viewRelativeStatus (actorLoc, actor) (alarmLoc, SmokeDetector _)
      = viewSharedInformation () [ViewWith mkView] myMap @! ()
      where
      mkView curMap
        = mkTable [ "Object Description", "Located in Room",     "Distance from " <+++ actor.userName]
                  [ ("Smoke Alarm !! ",   roomToString alarmLoc, spToDistString2 (shipShortestPath actorLoc alarmLoc curMap))
                  ]
  viewRelativeStatus (actorLoc, actor) (alarmLoc, FloodDetector _)
      = viewSharedInformation () [ViewWith mkView] myMap @! ()
      where
      mkView curMap
        # (_,pCost,nrPlugs, (plugLoc, distPlugs, _)) = smartShipPathToClosestObject Plug actorLoc alarmLoc curMap
        = mkTable [ "Object Description",                             "Located in Room",     "Distance from " <+++ actor.userName]
                  [ ("Flood Alarm !! ",                               roomToString alarmLoc, spToDistString2 (shipShortestPath actorLoc alarmLoc curMap), "Route length")
                  , ("Closest plug (" <+++ nrPlugs <+++ " in reach)", roomToString plugLoc,  roomToString distPlugs,                                      toString pCost)
                  ]

handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), priority)
  =   updStatusOfActor actor.userName Busy myMap
  >>| addLog ("Commander " <+++ me) actor.userName (message "Start Handling ")
  >>| appendTopLevelTaskPrioFor me (message "Wait for ") "High" True
        (handleWhileWalking actor (message "Handle ") (toSingleLineText priority)
        (taskToDo (alarmLoc, detector)) ) @! ()
  where
  message work = (work <+++ toString detector <+++ " in Room " <+++ alarmLoc)

  handleWhileWalking :: MyActor String String (MyActor MyRoom MyMap -> Task (Maybe (Task (Maybe String)))) -> Task ()
  handleWhileWalking actor title priority task
    =                   (((actor.userName, title) @: (              moveAround mkRoom actor (Just task) myMap
                                                     >>= \mbTask -> fromJust mbTask))

                        -||-
                        (viewInformation ("Cancel task \"" <+++ title <+++ "\"") [] () @! Nothing))
    >>= \mba -> if (isNothing mba)
                  (viewInformation ("Task " <+++ title <+++ " has been cancelled by you") [] ())
                  (viewInformation ("Task " <+++ title <+++ " terminated normally, returning:") [] (fromJust mba) @! ())
    >>|         return ()

  taskToDo :: (RoomNumber,Detector) MyActor MyRoom MyMap -> Task (Maybe (Task (Maybe String)))
  taskToDo (alarmLoc,detector) curActor curRoom curMap
    = viewInformation ("Handle " <+++ toString detector <+++ " in Room: " <+++ alarmLoc) []  ()
      -||
      (let path                                                 = shipShortestPath curRoom.number alarmLoc curMap
           (_,_,nrExt, (extLoc, distExt, dirExt))               = smartShipPathToClosestObject FireExtinguisher curRoom.number alarmLoc curMap
           (_,_,nrBlankets, (blanketLoc, distBlankets, dirBlanket)) = smartShipPathToClosestObject Blanket curRoom.number alarmLoc curMap
           (_,_,nrPlugs, (plugLoc, distPlugs, dirPlug))             = smartShipPathToClosestObject Plug curRoom.number alarmLoc curMap
      in viewInformation "" []
            (mkTable [ "Object Description", 								"Located in Room", 		"Distance from " <+++ curActor.userName, "Take Exit"]
               [ (toString detector, 										roomToString alarmLoc, 	spToDistString2 path, 					goto2 path)
               , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)", 	roomToString extLoc, 	roomToString distExt,					goto dirExt)
               , ("Closest Blanket ("	<+++ nrBlankets	<+++ " in reach)", 	roomToString blanketLoc,roomToString distBlankets,				goto dirBlanket)
               , ("Closest plug (" <+++ nrPlugs <+++ " in reach)", 			roomToString plugLoc, 	roomToString distPlugs,					goto dirPlug)
               ])) @! ()
      >>* [ OnAction (Action "Use Fire Extinguisher" []) (ifCond (mayUseExtinguisher detector) (return (Just useExtinquisher)))
          , OnAction (Action "Use Blanket" [])           (ifCond (mayUseBlanket detector)      (return (Just useBlanket)))
          , OnAction (Action "Use Plug" [])              (ifCond (mayUsePlug detector)         (return (Just usePlug)))
          , OnAction (Action "Smoke Investigated" [])    (ifCond (mayDetectedSmoke detector)   (return (Just smokeReport)))
          , OnAction (Action "I give up" [])             (always (return (Just giveUp)))
          ]
    where
    mayUseExtinguisher (FireDetector True) = curRoom.number == alarmLoc && (isMember FireExtinguisher curActor.carrying)
    mayUseExtinguisher _                   = False

    mayUseBlanket (FireDetector True)      = curRoom.number == alarmLoc && (isMember Blanket curActor.carrying)
    mayUseBlanket _                        = False

    mayUsePlug  (FloodDetector True)       = curRoom.number == alarmLoc && (isMember Plug curActor.carrying)
    mayUsePlug _                           = False

    mayDetectedSmoke (SmokeDetector True)  = curRoom.number == alarmLoc
    mayDetectedSmoke _                     = False

    useExtinquisher =   useObject alarmLoc FireExtinguisher curActor myMap
                    >>| setAlarm actor.userName (alarmLoc,detector) False myMap
                    >>| updStatusOfActor curActor.userName Available myMap
                    >>| viewInformation "Well Done, Fire Extinguished !" [] ()
                    >>| return (Just "Fire Extinguised")

    useBlanket      =   useObject alarmLoc Blanket curActor myMap
                    >>| setAlarm actor.userName (alarmLoc,detector) False myMap
                    >>| updStatusOfActor curActor.userName Available myMap
                    >>| viewInformation "Well Done, Fire Extinguished !" [] ()
                    >>| return (Just "Fire Extinguised")

    usePlug         =   useObject alarmLoc Plug curActor myMap
                    >>| setAlarm actor.userName (alarmLoc,detector) False myMap
                    >>| updStatusOfActor curActor.userName Available myMap
                    >>| viewInformation "Well Done, Flooding Stopped !" [] ()
                    >>| return (Just "Flooding Stopped")

    smokeReport     =   setAlarm actor.userName (alarmLoc,detector) False myMap
                    >>| updStatusOfActor curActor.userName Available myMap
                    >>| viewInformation "Well Done, Reason of Smoke Detected !" [] ()
                    >>| return (Just "Don't smoke under a smoke detector !!")

    giveUp          =   updStatusOfActor curActor.userName Available myMap
                    >>| return (Just "I gave up, send somebody else...")

goto Nothing      = "Unreachable!"
goto (Just []) 	  = "you are already in the target room"
goto (Just (dir)) = toString (hd dir)

goto2 Nothing      = "Unreachable!"
goto2 (Just ([],_)) 	  = "you are already in the target room"
goto2 (Just (dir,_)) = toString (hd dir)


updStatusOfActor :: User Availability (Shared MyMap) -> Task ()
updStatusOfActor user availability smap
  =   updActorStatus user (\st -> {st & occupied = availability}) smap
  >>| addLog user "" ("Has become " <+++ availability)

autoHandleWithScript :: (User,(RoomNumber,Detector),(RoomNumber,MyActor),Priority) -> Task ()
autoHandleWithScript  (commander,(alarmLoc,detector),(actorLoc,actor),prio)
  =              case detector of
  					(FireDetector   _) -> get handleFireScript
  					(FloodDetector  _) -> get handleFloodScript
  					(SmokeDetector  _) -> get handleSmokeScript
  >>= \script -> appendTopLevelTaskPrioFor actor.userName ("Auto script " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True 
                 (   updStatusOfActor actor.userName  Busy myMap 
                 >>| addLog ("Commander " <+++ commander) actor.userName ("Simulate Handling " <+++ toString detector <+++ " detected in " <+++ alarmLoc)
                 >>| interperScript (alarmLoc,detector) actor.userName script // perform script (actorLoc,actor)
                 >>| updStatusOfActor actor.userName  Available myMap 
                 >>| addLog actor.userName commander  ("Simulation Handling " <+++ toString detector <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
                 ) @! ()

// simulate via auto stuf

autoHandleAlarm commander user (alarmLoc,detector) 
  = appendTopLevelTaskPrioFor user ("Auto handling " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True 
      (startSimulation commander user (alarmLoc,detector)) @! ()

startSimulation :: User User (RoomNumber,Detector) -> Task Bool
startSimulation commander user (alarmLoc,detector) 
  =              updStatusOfActor user Busy myMap 
  >>|            addLog ("Commander " <+++ commander) user ("Simulate Handling " <+++ toString detector <+++ " detected in " <+++ alarmLoc)
  >>|            get myMap 
  >>= \curMap -> let  (myLoc,curActor)       = fromJust (findUser user curMap) 						
                      (mbObjectLoc,mbObject) = findClosestObject myLoc (alarmLoc,detector) curMap		
                 in if (isNothing mbObjectLoc)
                      (return False)
                      if (isNothing mbObject)
                        (simulateHandling myLoc alarmLoc detector curActor myMap)
                        (simulateHandlingWithObject myLoc (fromJust mbObject) (fromJust mbObjectLoc) alarmLoc detector curActor myMap) 
  >>= \succes -> updStatusOfActor user Available myMap 
  >>|            addLog user commander  ("Simulation Handling " <+++ toString detector <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
  >>|            return True

simulateHandling startLoc alarmLoc detector actor smap
  =                     autoMove startLoc alarmLoc shipShortestPath actor smap
  >>= \targetReached -> if targetReached
                          (setAlarm actor.userName (alarmLoc,detector) False smap @! True)
                          (return False)

simulateHandlingWithObject startLoc object objectLoc alarmLoc detector actor smap
  =                     addLog actor.userName "Started Simulation" ("From " <+++ startLoc <+++ 
  																	" to " <+++	alarmLoc <+++ 
  																	" via " <+++ objectLoc <+++
  																	" fetching " <+++ object)
  >>|					autoMove startLoc objectLoc shipShortestPath actor smap
  >>= \objectReached -> if objectReached (pickupObject objectLoc object actor smap
  >>= \objectFound   -> if objectFound   (autoMove objectLoc alarmLoc shipShortestPath actor smap
  >>= \targetReached -> if targetReached (useObject alarmLoc object actor smap
  >>= \used          -> if used          (setAlarm actor.userName (alarmLoc,detector) False smap @! True)
                                         (return False))
                                         (return False))
                                         (return False))
                                         (return False)

findClosestObject :: RoomNumber (RoomNumber,Detector) MyMap -> (Maybe RoomNumber,Maybe Object)
findClosestObject  myLoc (alarmLoc, detector) curMap
  = case detector of
      (SmokeDetector _) = (Just myLoc, Nothing)
      (FloodDetector _) = case findClosest myLoc alarmLoc Plug curMap of
                            Nothing -> (Nothing, Nothing)
                            objLoc  -> (Just (snd (fromJust objLoc)), Just Plug)
      (FireDetector _)  
      # fireLoc 	= findClosest myLoc alarmLoc FireExtinguisher curMap
      # blanketLoc 	= findClosest myLoc alarmLoc Blanket curMap
	  | isNothing fireLoc && isNothing blanketLoc 	= (Nothing,Nothing)
	  | isNothing blanketLoc 						= (Just (snd (fromJust fireLoc)), Just FireExtinguisher)
	  | isNothing fireLoc 							= (Just (snd (fromJust blanketLoc)), Just Blanket)
	  = if (less (fromJust fireLoc) (fromJust blanketLoc))
        		(Just (snd (fromJust fireLoc)), Just FireExtinguisher)
        		(Just (snd (fromJust blanketLoc)), Just Blanket)
where
	less (c1,_) (c2,_) 
	| c1 >=0 && c2 >= 0 = c1<c2
	
from StdMisc import abort

findClosest :: RoomNumber RoomNumber Object MyMap -> Maybe (Int, RoomNumber)
findClosest myLoc targetLoc object curMap
  = case smartShipPathToClosestObject object myLoc targetLoc curMap of
      (cost,_,_, (_, distance, Just path))
        = case reverse path of
            []    -> Just (cost, myLoc)
            [x:_] -> Just (cost, fromExit x)
      _ = Nothing

mkRoom :: MyRoom -> Task ()
mkRoom room = viewInformation "Room Status" [imageView (\(room, _) -> roomImage True (Just room)) (\_ _ -> Nothing)] (room, NoMapClick) @! ()

