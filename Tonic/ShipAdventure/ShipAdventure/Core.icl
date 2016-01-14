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
import ShipAdventure.PathFinding, ShipAdventure.Util

// the next function should be placed in the library somewhere

mkTable :: [String]  ![a] -> Table | gText{|*|} a
mkTable	headers a = Table headers (map row a) Nothing
  where
  row x = [Text cell \\ cell <- gText{|*|} AsRow (Just x)]

myTasks :: [Workflow]
myTasks = [ workflow "walk around"  "enter map, walk around, follow instructions of commander" currentUserWalkAround
          , workflow "commander"    "give instructions to crew members on the map"             giveInstructions
          , workflow "alter fire script" "define your own script for handling fires"           changeFireScript
          , workflow "alter flood script" "define your own script for handling floods"         changeFloodScript
          , workflow "alter smoke script" "define your own script for handling smoke"          changeSmokeScript
          ]

currentUserWalkAround :: Task ()
currentUserWalkAround = get currentUser >>= actorWithInstructions

// initial task to place an actor on the map
// one can only assign tasks to actors on the map

actorWithInstructions :: User -> Task ()
actorWithInstructions user
  # actor = newActor user
  =           get myActorMap
  >>= \mam -> case findUser actor.userName mam of
                Nothing
                  =           enterInformation "Which room do you want to start in?" []
                  >>= \loc -> addLog user "" ("Entered the building starting in room " <+++ loc)
                  >>|         addActorToMap mkRoom actor loc myStatusMap myActorMap myInventoryMap myMap
                Just (_, me) = moveAround mkRoom me noTask myStatusMap myActorMap myInventoryMap myMap @! ()
  where
  newActor user
    = {userName = user, carrying = [], actorStatus = {occupied = Available}}

  noTask :: Maybe (ActorTask r o a ()) | iTask r & iTask o & iTask a & Eq o
  noTask = Nothing

// given the alarms one has to decide which tasks to assign to handle the situation

spToDistString :: (Maybe [Exit]) -> String
spToDistString (Just es) = toString (length es)
spToDistString _         = "Room unreachable!"

spToDistString2 :: (Maybe ([Exit],Distance)) -> String
spToDistString2 (Just (es,_)) = toString (length es)
spToDistString2 _             = "Room unreachable!"

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
             \(alarmLoc, detector) -> scriptDefined detector
             >>= \scriptExists ->     selectSomeOneToHandle (alarmLoc, detector)
             >&>                      withSelection (viewInformation () [] "No Crew Member Selected")
             \(actorLoc, actor) ->    viewRelativeStatus (actorLoc, actor) (alarmLoc, detector)
                                      ||-
                                      updateChoice "Select the Priority : " [ChooseWith (ChooseFromRadioButtons id)] [Low, Normal, High, Highest] High
             >>* [ OnAction ActionByHand    (hasValue (\prio -> handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), prio)))
                 , OnAction ActionSimulated (hasValue (\prio -> autoHandleAlarm me actor.userName (alarmLoc, detector) @! ()))
//                 , OnAction ActionScript    (hasValue (\prio -> autoHandleWithScript (me, (alarmLoc, detector), (actorLoc, actor), prio) @! ()))
                 , OnAction ActionScript    (ifValue (\_ -> scriptExists) (\prio -> autoHandleWithScript (me, (alarmLoc, detector), (actorLoc, actor), prio) @! ()))
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
      = viewSharedInformation () [ViewWith mkView] (myStatusMap |+| myInventoryMap |+| exitLockShare) @! ()
      where
      mkView ((statusMap, inventoryMap), exitLocks)
        # (_,_,eCost,nrExt, (extLoc, distExt, _))                       = smartShipPathToClosestObject FireExtinguisher actorLoc alarmLoc statusMap inventoryMap exitLocks myMap
        # (_,_,bCost,nrFireBlankets, (blanketLoc, distFireBlankets, _)) = smartShipPathToClosestObject FireBlanket      actorLoc alarmLoc statusMap inventoryMap exitLocks myMap
        # fireDist                                                      = shipShortestPath actorLoc alarmLoc statusMap exitLocks myMap
        = mkTable [ "Object Description",                                           "Located in Room" ,      "Distance from " <+++ actor.userName, "Route Length"]
                  [ ("Fire Alarm !! " ,                                             roomToString alarmLoc,   spToDistString2 fireDist,             spToDistString2 fireDist)
                  , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)",         roomToString extLoc,     roomToString distExt,                 toString eCost)
                  , ("Closest FireBlanket (" <+++ nrFireBlankets <+++ " in reach)", roomToString blanketLoc, roomToString distFireBlankets,        toString bCost)
                  ]
  viewRelativeStatus (actorLoc, actor) (alarmLoc, SmokeDetector _)
      = viewSharedInformation () [ViewWith mkView] (myStatusMap |+| myInventoryMap |+| exitLockShare) @! ()
      where
      mkView ((statusMap, inventoryMap), exitLocks)
        # distance = shipShortestPath actorLoc alarmLoc statusMap exitLocks myMap
        = mkTable [ "Object Description", "Located in Room",     "Distance from " <+++ actor.userName, "Route Length"]
                  [ ("Smoke Alarm !! ",   roomToString alarmLoc, spToDistString2 distance, spToDistString2 distance )
                  ]
  viewRelativeStatus (actorLoc, actor) (alarmLoc, FloodDetector _)
    = viewSharedInformation () [ViewWith mkView] (myStatusMap |+| myInventoryMap |+| exitLockShare) @! ()
    where
    mkView ((statusMap, inventoryMap), exitLocks)
      # (_,_,pCost,nrPlugs, (plugLoc, distPlugs, _)) = smartShipPathToClosestObject Plug actorLoc alarmLoc statusMap inventoryMap exitLocks myMap
      # floodDist                                    = shipShortestPath actorLoc alarmLoc statusMap exitLocks myMap
      = mkTable [ "Object Description",                             "Located in Room",     "Distance from " <+++ actor.userName, "Route Length"]
                [ ("Flood Alarm !! ",                               roomToString alarmLoc, spToDistString2 floodDist,            spToDistString2 floodDist)
                , ("Closest plug (" <+++ nrPlugs <+++ " in reach)", roomToString plugLoc,  roomToString distPlugs,               toString pCost)
                ]

handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), priority)
  =   updStatusOfActor actor.userName Busy
  >>| addLog ("Commander " <+++ me) actor.userName (message "Start Handling ")
  >>| appendTopLevelTaskPrioFor me (message "Wait for ") "High" True
        (handleWhileWalking actor (message "Handle ") (toSingleLineText priority)
          (taskToDo (alarmLoc, detector)) ) @! ()
  where
  message work = (work <+++ toString detector <+++ " in Room " <+++ alarmLoc)

  handleWhileWalking :: MyActor String String
                        (MyActor Room MyRoomStatusMap MyRoomActorMap MyRoomInventoryMap DungeonMap -> Task (Maybe (Task (Maybe String))))
                     -> Task ()
  handleWhileWalking actor title priority task
    =           (((actor.userName, title) @: (              moveAround mkRoom actor (Just task) myStatusMap myActorMap myInventoryMap myMap
                                             >>= \mbTask -> fromJust mbTask))

                -||-
                (viewInformation ("Cancel task \"" <+++ title <+++ "\"") [] () @! Nothing))
    >>= \mba -> if (isNothing mba)
                  (viewInformation ("Task " <+++ title <+++ " has been cancelled by you") [] ())
                  (viewInformation ("Task " <+++ title <+++ " terminated normally, returning:") [] (fromJust mba) @! ())
    >>|         return ()

  taskToDo :: (RoomNumber,Detector) MyActor Room MyRoomStatusMap MyRoomActorMap MyRoomInventoryMap DungeonMap -> Task (Maybe (Task (Maybe String)))
  taskToDo (alarmLoc,detector) curActor curRoom statusMap actorMap inventoryMap curMap
    =                 get exitLockShare
    >>= \exitLocks -> (viewInformation ("Handle " <+++ toString detector <+++ " in Room: " <+++ alarmLoc) []  ()
                      -||
                      (let path                                                                          = shipShortestPath curRoom.number alarmLoc statusMap exitLocks curMap
                           (_, _, eCost, nrExt,          (extLoc, distExt, dirExt))                      = smartShipPathToClosestObject FireExtinguisher curRoom.number alarmLoc statusMap inventoryMap exitLocks curMap
                           (_, _, bCost, nrFireBlankets, (blanketLoc, distFireBlankets, dirFireBlanket)) = smartShipPathToClosestObject FireBlanket curRoom.number alarmLoc statusMap inventoryMap exitLocks curMap
                           (_, _, pCost, nrPlugs,        (plugLoc, distPlugs, dirPlug))                  = smartShipPathToClosestObject Plug curRoom.number alarmLoc statusMap inventoryMap exitLocks curMap
                      in viewInformation "" []
                            (mkTable [ "Object Description",                                     "Located in Room",       "Take Exit",         "Distance from " <+++ curActor.userName, "Route Length"]
                               [ (toString detector,                                             roomToString alarmLoc,   goto2 path,          spToDistString2 path,                    spToDistString2 path)
                               , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)",         roomToString extLoc,     goto dirExt,         roomToString distExt,                    toString eCost)
                               , ("Closest FireBlanket (" <+++ nrFireBlankets <+++ " in reach)", roomToString blanketLoc, goto dirFireBlanket, roomToString distFireBlankets,           toString bCost)
                               , ("Closest plug (" <+++ nrPlugs <+++ " in reach)",               roomToString plugLoc,    goto dirPlug,        roomToString distPlugs,                  toString pCost)
                               ])) @! ()
                      >>* [ /* TODO FIXME Needs to be dynamic and specific to the individual resources being carried OnAction (Action "Use Fire Extinguisher" []) (ifCond (mayUseExtinguisher detector) (return (Just useExtinquisher)))
                          , OnAction (Action "Use FireBlanket" [])       (ifCond (mayUseFireBlanket detector) (return (Just useFireBlanket)))
                          , OnAction (Action "Use Plug" [])              (ifCond (mayUsePlug detector)        (return (Just usePlug)))
                          ,*/ OnAction (Action "Smoke Investigated" [])  (ifCond (mayDetectedSmoke detector)  (return (Just smokeReport)))
                          , OnAction (Action "I give up" [])             (always (return (Just giveUp)))
                          ])
    where
    mayUseExtinguisher (FireDetector True) = curRoom.number == alarmLoc && isCarrying FireExtinguisher curActor
    mayUseExtinguisher _                   = False

    mayUseFireBlanket (FireDetector True)  = curRoom.number == alarmLoc && isCarrying FireBlanket curActor
    mayUseFireBlanket _                    = False

    mayUsePlug (FloodDetector True)        = curRoom.number == alarmLoc && isCarrying Plug curActor
    mayUsePlug _                           = False

    mayDetectedSmoke (SmokeDetector True)  = curRoom.number == alarmLoc
    mayDetectedSmoke _                     = False

    //useExtinquisher =   useObject alarmLoc FireExtinguisher curActor myMap
                    //>>| setAlarm actor.userName (alarmLoc,detector) False myStatusMap
                    //>>| updStatusOfActor curActor.userName Available
                    //>>| viewInformation "Well Done, Fire Extinguished !" [] ()
                    //>>| return (Just "Fire Extinguised")

    //useFireBlanket      =   useObject alarmLoc FireBlanket curActor myMap
                    //>>| setAlarm actor.userName (alarmLoc,detector) False myStatusMap
                    //>>| updStatusOfActor curActor.userName Available
                    //>>| viewInformation "Well Done, Fire Extinguished !" [] ()
                    //>>| return (Just "Fire Extinguised")

    //usePlug         =   useObject alarmLoc Plug curActor myMap
                    //>>| setAlarm actor.userName (alarmLoc,detector) False myMap
                    //>>| updStatusOfActor curActor.userName Available
                    //>>| viewInformation "Well Done, Flooding Stopped !" [] ()
                    //>>| return (Just "Flooding Stopped")

    smokeReport     =   setAlarm actor.userName (alarmLoc,detector) False myStatusMap
                    >>| updStatusOfActor curActor.userName Available
                    >>| viewInformation "Well Done, Reason of Smoke Detected !" [] ()
                    >>| return (Just "Don't smoke under a smoke detector !!")

    giveUp          =   updStatusOfActor curActor.userName Available
                    >>| return (Just "I gave up, send somebody else...")

goto Nothing      = "Unreachable!"
goto (Just [])    = "you are already in the target room"
goto (Just (dir)) = toString (hd dir)

goto2 Nothing        = "Unreachable!"
goto2 (Just ([],_))  = "you are already in the target room"
goto2 (Just (dir,_)) = toString (hd dir)

updStatusOfActor :: User Availability -> Task ()
updStatusOfActor user availability
  =   updActorStatus user (\st -> {st & occupied = availability}) myActorMap
  >>| addLog user "" ("Has become " <+++ availability)

scriptDefined :: Detector -> Task Bool
scriptDefined detector
  = case detector of
      (FireDetector   _) -> get handleFireScript  >>= \script -> return (length script > 0)
      (FloodDetector  _) -> get handleFloodScript >>= \script -> return (length script > 0)
      (SmokeDetector  _) -> get handleSmokeScript >>= \script -> return (length script > 0)

autoHandleWithScript :: (User, (RoomNumber, Detector), (RoomNumber, MyActor), Priority) -> Task ()
autoHandleWithScript  (commander,(alarmLoc,detector),(actorLoc,actor),prio)
  =              case detector of
                   (FireDetector   _) -> get handleFireScript
                   (FloodDetector  _) -> get handleFloodScript
                   (SmokeDetector  _) -> get handleSmokeScript
  >>= \script -> appendTopLevelTaskPrioFor actor.userName ("Auto script " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True
                 (   updStatusOfActor actor.userName Busy
                 >>| addLog ("Commander " <+++ commander) actor.userName ("Simulate Handling " <+++ toString detector <+++ " detected in " <+++ alarmLoc)
                 >>| interperScript (alarmLoc,detector) actor.userName script // perform script (actorLoc,actor)
                 >>| updStatusOfActor actor.userName Available
                 >>| addLog actor.userName commander ("Simulation Handling " <+++ toString detector <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
                 ) @! ()

// simulate via auto stuf

autoHandleAlarm commander user (alarmLoc,detector)
  = appendTopLevelTaskPrioFor user ("Auto handling " <+++ toString detector <+++ " in room " <+++ alarmLoc) "High" True
      (startSimulation commander user (alarmLoc, detector)) @! ()

startSimulation :: User User (RoomNumber, Detector) -> Task Bool
startSimulation commander user (alarmLoc, detector)
  =                 updStatusOfActor user Busy
  >>|               addLog ("Commander " <+++ commander) user ("Simulate Handling " <+++ toString detector <+++ " detected in " <+++ alarmLoc)
  >>|               get myStatusMap
  >>= \statusMap -> get myActorMap
  >>= \mam       -> get exitLockShare
  >>= \exitLocks -> get myInventoryMap
  >>= \invMap    -> let (myLoc, curActor) = fromJust (findUser user mam)
                    in  case findClosestObject myLoc (alarmLoc, detector) statusMap invMap exitLocks myMap of
                          (Nothing, _) = endSimulation False
                          (Just loc, mbObj) = (case mbObj of
                                                 Nothing  = simulateHandling myLoc alarmLoc detector curActor myStatusMap myMap
                                                 Just obj = simulateHandlingWithObject myLoc obj loc alarmLoc detector curActor myStatusMap myInventoryMap myMap
                                              )
                                              >>| endSimulation True
  where
  endSimulation ok
    =   updStatusOfActor user Available
    >>| addLog user commander  ("Simulation Handling " <+++ toString detector <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
    >>| return ok


simulateHandling :: RoomNumber RoomNumber Detector MyActor (Shared MyRoomStatusMap) DungeonMap -> Task Bool
simulateHandling startLoc alarmLoc detector actor myStatusMap dungeonMap
  =                     autoMove startLoc alarmLoc shipShortestPath actor myStatusMap myActorMap dungeonMap
  >>= \targetReached -> if targetReached
                          (setAlarm actor.userName (alarmLoc, detector) False myStatusMap @! True)
                          (return False)

simulateHandlingWithObject :: RoomNumber MyObject RoomNumber RoomNumber Detector MyActor (Shared MyRoomStatusMap) (Shared MyRoomInventoryMap) DungeonMap
                           -> Task Bool
simulateHandlingWithObject startLoc object objectLoc alarmLoc detector actor myStatusMap myInventoryMap dungeonMap
  =                     addLog actor.userName "Started Simulation" ("From " <+++ startLoc <+++
                                                                    " to " <+++	alarmLoc <+++
                                                                    " via " <+++ objectLoc <+++
                                                                    " fetching " <+++ object)
  >>|                   autoMove startLoc objectLoc shipShortestPath actor myStatusMap myActorMap dungeonMap
  >>= \objectReached -> if objectReached (pickupObject objectLoc object actor myActorMap myInventoryMap
  >>= \objectFound   -> if objectFound   (autoMove objectLoc alarmLoc shipShortestPath actor myStatusMap myActorMap dungeonMap
  >>= \targetReached -> if targetReached (useObject alarmLoc object actor myActorMap
  >>= \used          -> if used          (setAlarm actor.userName (alarmLoc,detector) False myStatusMap @! True)
                                         (return False))
                                         (return False))
                                         (return False))
                                         (return False)

findClosestObject :: RoomNumber (RoomNumber, Detector) MyRoomStatusMap MyRoomInventoryMap RoomExitLockMap DungeonMap -> (Maybe RoomNumber, Maybe MyObject)
findClosestObject  myLoc (alarmLoc, detector) statusMap inventoryMap exitLocks curMap
  = case detector of
      (SmokeDetector _) = (Just myLoc, Nothing)
      (FloodDetector _) = case findClosest myLoc alarmLoc Plug statusMap inventoryMap exitLocks curMap of
                            Nothing -> (Nothing, Nothing)
                            Just (obj, _, roomNo)  -> (Just roomNo, Just obj)
      (FireDetector _)
      # fireLoc 	= findClosest myLoc alarmLoc FireExtinguisher statusMap inventoryMap exitLocks curMap
      # blanketLoc 	= findClosest myLoc alarmLoc FireBlanket      statusMap inventoryMap exitLocks curMap
      = case (fireLoc, blanketLoc) of
          (Just (obj, _, roomNo), Nothing) -> (Just roomNo, Just obj)
          (Nothing, Just (obj, _, roomNo)) -> (Just roomNo, Just obj)
          (Just (obj1, dist1, roomNo1), Just (obj2, dist2, roomNo2))
            | less dist1 dist2 -> (Just roomNo1, Just obj1)
            | otherwise        -> (Just roomNo2, Just obj2)
          _                    -> (Nothing, Nothing)
  where
  less c1 c2
  | c1 >=0 && c2 >= 0 = c1<c2

from StdMisc import abort

findClosest :: RoomNumber RoomNumber ObjectType MyRoomStatusMap MyRoomInventoryMap RoomExitLockMap DungeonMap -> Maybe (MyObject, Int, RoomNumber)
findClosest myLoc targetLoc objectType statusMap inventoryMap exitLocks dungeonMap
  = case smartShipPathToClosestObject objectType myLoc targetLoc statusMap inventoryMap exitLocks dungeonMap of
      (Just obj, cost,_,_, (_, distance, Just path))
        = case reverse path of
            []    -> Just (obj, cost, myLoc)
            [x:_] -> Just (obj, cost, fromExit x)
      _ = Nothing

mkRoom :: Room -> Task ()
mkRoom room = viewInformation "Room Status" [imageView (\(room, _) -> roomImage True (Just room)) (\_ _ -> Nothing)] (room, NoMapClick) @! ()

