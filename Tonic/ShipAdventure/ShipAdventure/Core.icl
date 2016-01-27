implementation module ShipAdventure.Core

import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data
import qualified Data.IntMap.Strict as DIS

import ShipAdventure.Types, Adventure.Logging, ShipAdventure.Scripting
import ShipAdventure.PathFinding, ShipAdventure.Util

// the next function should be placed in the library somewhere

mkTable :: [String] ![a] -> Table | gText{|*|} a
mkTable	headers a = Table headers (map row a) Nothing
  where
  row x = [Text cell \\ cell <- gText{|*|} AsRow (Just x)]

myTasks :: [Workflow]
myTasks = [ workflow "Walk around"  "Enter map, walk around, follow instructions of commander" currentUserWalkAround
          , workflow "D-Officer"    "Give instructions to crew members on the map"             giveInstructions
          , workflow "Alter fire script" "Define your own script for handling fires"           changeFireScript
          , workflow "Alter flood script" "Define your own script for handling floods"         changeFloodScript
          , workflow "Alter smoke script" "Define your own script for handling smoke"          changeSmokeScript
          ]

currentUserWalkAround :: Task ()
currentUserWalkAround = get currentUser >>= actorWithInstructions

// initial task to place an actor on the map
// one can only assign tasks to actors on the map

actorWithInstructions :: User -> Task ()
actorWithInstructions user
  # actor = newActor user
  =           get myActorMap
  >>= \mam -> get myNetwork
  >>= \nw  -> case findUser actor.userName mam of
                Nothing
                  =           enterInformation "Which room do you want to start in?" []
                  >>= \loc -> addLog user "" ("Entered the building starting in room " <+++ loc)
                  >>|         addActorToMap (mkRoom nw) actor loc myStatusMap myActorMap myInventoryMap myMap
                Just (_, me) = moveAround (mkRoom nw) me noTask myStatusMap myActorMap myInventoryMap myMap @! ()
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
                 //, OnAction ActionScript    (hasValue (\prio -> autoHandleWithScript (me, (alarmLoc, detector), (actorLoc, actor), prio) @! ()))
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

  selectSomeOneToHandle :: (RoomNumber, RoomStatus) -> Task (Int, MyActor)
  selectSomeOneToHandle (number, detector)
    = enterChoiceWithShared ("Who should handle: " <+++ showAlarm (number, detector))
        [ChooseWith (ChooseFromGrid (\(roomNumber,actor) -> (roomNumber, actor.userName, actor.actorStatus)))] allAvailableActors

  viewRelativeStatus :: (RoomNumber, MyActor) (RoomNumber, RoomStatus) -> Task ()
  viewRelativeStatus (actorLoc, actor) (alarmLoc, status)
    # view = if (hasFire status) mkFireView
               (if (hasSmoke status) mkSmokeView
                  (if (hasWater status) mkWaterView
                     (\_ -> mkTable ["Status"] ["Everything in order"])
                  )
               )
    = viewSharedInformation () [ViewWith view] (myStatusMap |+| myInventoryMap |+| exitLockShare) @! ()
    where
    mkFireView ((statusMap, inventoryMap), exitLocks)
      # (_,_,eCost,nrExt, (extLoc, distExt, _))                       = smartShipPathToClosestObject FireExtinguisher actorLoc alarmLoc statusMap inventoryMap exitLocks myMap
      # (_,_,bCost,nrFireBlankets, (blanketLoc, distFireBlankets, _)) = smartShipPathToClosestObject FireBlanket      actorLoc alarmLoc statusMap inventoryMap exitLocks myMap
      # fireDist                                                      = shipShortestPath actorLoc alarmLoc statusMap exitLocks myMap
      = mkTable [ "Object Description",                                           "Located in Room" ,      "Distance from " <+++ actor.userName, "Route Length"]
                [ ("Fire Alarm !! " ,                                             roomToString alarmLoc,   spToDistString2 fireDist,             spToDistString2 fireDist)
                , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)",         roomToString extLoc,     roomToString distExt,                 toString eCost)
                , ("Closest FireBlanket (" <+++ nrFireBlankets <+++ " in reach)", roomToString blanketLoc, roomToString distFireBlankets,        toString bCost)
                ]
    mkSmokeView ((statusMap, inventoryMap), exitLocks)
      # distance = shipShortestPath actorLoc alarmLoc statusMap exitLocks myMap
      = mkTable [ "Object Description", "Located in Room",     "Distance from " <+++ actor.userName, "Route Length"]
                [ ("Smoke Alarm !! ",   roomToString alarmLoc, spToDistString2 distance, spToDistString2 distance )
                ]
    mkWaterView ((statusMap, inventoryMap), exitLocks)
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
    =           get myNetwork
    >>= \nw ->  (((actor.userName, title) @: (              moveAround (mkRoom nw) actor (Just task) myStatusMap myActorMap myInventoryMap myMap
                                             >>= \mbTask -> case mbTask of
                                                              Just t -> t
                                                              _      -> viewInformation "Error" [] "handleWhileWalking (1)" @! Nothing
                                             ))

                -||-
                (viewInformation ("Cancel task \"" <+++ title <+++ "\"") [] () @! Nothing))
    >>= \mba -> case mba of
                  Just ba -> viewInformation ("Task " <+++ title <+++ " terminated normally, returning:") [] ba @! ()
                  _ -> viewInformation ("Task " <+++ title <+++ " has been cancelled by you") [] ()
    >>|         return ()

  taskToDo :: (RoomNumber, RoomStatus) MyActor Room MyRoomStatusMap MyRoomActorMap MyRoomInventoryMap DungeonMap -> Task (Maybe (Task (Maybe String)))
  taskToDo (alarmLoc, status) curActor curRoom statusMap actorMap inventoryMap curMap
    =                 get exitLockShare
    >>= \exitLocks -> (viewInformation ("Handle " <+++ toString status <+++ " in Room: " <+++ alarmLoc) []  ()
                      -||
                      (let path                                                                          = shipShortestPath curRoom.number alarmLoc statusMap exitLocks curMap
                           (_, _, eCost, nrExt,          (extLoc, distExt, dirExt))                      = smartShipPathToClosestObject FireExtinguisher curRoom.number alarmLoc statusMap inventoryMap exitLocks curMap
                           (_, _, bCost, nrFireBlankets, (blanketLoc, distFireBlankets, dirFireBlanket)) = smartShipPathToClosestObject FireBlanket curRoom.number alarmLoc statusMap inventoryMap exitLocks curMap
                           (_, _, pCost, nrPlugs,        (plugLoc, distPlugs, dirPlug))                  = smartShipPathToClosestObject Plug curRoom.number alarmLoc statusMap inventoryMap exitLocks curMap
                      in viewInformation "" []
                            (mkTable [ "Object Description",                                     "Located in Room",       "Take Exit",         "Distance from " <+++ curActor.userName, "Route Length"]
                               [ (toString status,                                             roomToString alarmLoc,   goto2 path,          spToDistString2 path,                    spToDistString2 path)
                               , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)",         roomToString extLoc,     goto dirExt,         roomToString distExt,                    toString eCost)
                               , ("Closest FireBlanket (" <+++ nrFireBlankets <+++ " in reach)", roomToString blanketLoc, goto dirFireBlanket, roomToString distFireBlankets,           toString bCost)
                               , ("Closest plug (" <+++ nrPlugs <+++ " in reach)",               roomToString plugLoc,    goto dirPlug,        roomToString distPlugs,                  toString pCost)
                               ])) @! ()
                      >>* [ OnAction (Action "Use Fire Extinguisher" []) (ifCond (mayUseExtinguisher status) (return (Just useExtinquisher)))
                          , OnAction (Action "Use FireBlanket" [])       (ifCond (mayUseFireBlanket status)  (return (Just useFireBlanket)))
                          , OnAction (Action "Use Plug" [])              (ifCond (mayUsePlug status)         (return (Just usePlug)))
                          , OnAction (Action "Smoke Investigated" [])    (ifCond (mayDetectedSmoke status)   (return (Just smokeReport)))
                          , OnAction (Action "I give up" [])             (always (return (Just giveUp)))
                          ])
    where
    mayUseExtinguisher status = hasFire  status && curRoom.number == alarmLoc && isCarrying FireExtinguisher curActor
    mayUseFireBlanket  status = hasFire  status && curRoom.number == alarmLoc && isCarrying FireBlanket curActor
    mayUsePlug         status = hasWater status && curRoom.number == alarmLoc && isCarrying Plug curActor
    mayDetectedSmoke   status = hasSmoke status && curRoom.number == alarmLoc

    useExtinquisher =   useObject alarmLoc (getObjectOfType curActor FireExtinguisher) curActor myActorMap
                    >>| setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
                    >>| updStatusOfActor curActor.userName Available
                    >>| viewInformation "Well Done, Fire Extinguished !" [] ()
                    >>| return (Just "Fire Extinguised")

    useFireBlanket  =   useObject alarmLoc (getObjectOfType curActor FireBlanket) curActor myActorMap
                    >>| setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
                    >>| updStatusOfActor curActor.userName Available
                    >>| viewInformation "Well Done, Fire Extinguished !" [] ()
                    >>| return (Just "Fire Extinguised")

    usePlug         =   useObject alarmLoc (getObjectOfType curActor Plug) curActor myActorMap
                    >>| setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
                    >>| updStatusOfActor curActor.userName Available
                    >>| viewInformation "Well Done, Flooding Stopped !" [] ()
                    >>| return (Just "Flooding Stopped")

    smokeReport     =   setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
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

scriptDefined :: RoomStatus -> Task Bool
scriptDefined status
  | hasFire status  = get handleFireScript  >>= \script -> return (length script > 0)
  | hasWater status = get handleFloodScript >>= \script -> return (length script > 0)
  | hasSmoke status = get handleSmokeScript >>= \script -> return (length script > 0)
  | otherwise       = return False

autoHandleWithScript :: (User, (RoomNumber, RoomStatus), (RoomNumber, MyActor), Priority) -> Task ()
autoHandleWithScript (commander, (alarmLoc, status), (actorLoc, actor), prio)
  | hasFire status  = get handleFireScript  >>= continueWithScript
  | hasWater status = get handleFloodScript >>= continueWithScript
  | hasSmoke status = get handleSmokeScript >>= continueWithScript
  | otherwise       = return ()
  where
  continueWithScript script
    = appendTopLevelTaskPrioFor actor.userName ("Auto script " <+++ toString status <+++ " in room " <+++ alarmLoc) "High" True
      (   updStatusOfActor actor.userName Busy
      >>| addLog ("Commander " <+++ commander) actor.userName ("Simulate Handling " <+++ toString status <+++ " detected in " <+++ alarmLoc)
      >>| interperScript (alarmLoc, status) actor.userName script // perform script (actorLoc,actor)
      >>| updStatusOfActor actor.userName Available
      >>| addLog actor.userName commander ("Simulation Handling " <+++ toString status <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
      ) @! ()

// simulate via auto stuf

autoHandleAlarm commander user (alarmLoc, status)
  = appendTopLevelTaskPrioFor user ("Auto handling " <+++ toString status <+++ " in room " <+++ alarmLoc) "High" True
      (startSimulation commander user (alarmLoc, status)) @! ()

startSimulation :: User User (RoomNumber, RoomStatus) -> Task Bool
startSimulation commander user (alarmLoc, status)
  =                 updStatusOfActor user Busy
  >>|               addLog ("Commander " <+++ commander) user ("Simulate Handling " <+++ toString status <+++ " detected in " <+++ alarmLoc)
  >>|               get myStatusMap
  >>= \statusMap -> get myActorMap
  >>= \mam       -> get exitLockShare
  >>= \exitLocks -> get myInventoryMap
  >>= \invMap    -> case findUser user mam of
                      Just (myLoc, curActor)
                        = case findClosestObject myLoc (alarmLoc, status) statusMap invMap exitLocks myMap of
                            (Nothing, _) = endSimulation False
                            (Just loc, mbObj) = (case mbObj of
                                                   Nothing  = simulateHandling myLoc alarmLoc status curActor myStatusMap myMap
                                                   Just obj = simulateHandlingWithObject myLoc obj loc alarmLoc status curActor myStatusMap myInventoryMap myMap
                                                )
                                                >>| endSimulation True
                      _ = endSimulation False
  where
  endSimulation ok
    =   updStatusOfActor user Available
    >>| addLog user commander  ("Simulation Handling " <+++ toString status <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
    >>| return ok


simulateHandling :: RoomNumber RoomNumber RoomStatus MyActor (Shared MyRoomStatusMap) DungeonMap -> Task Bool
simulateHandling startLoc alarmLoc status actor myStatusMap dungeonMap
  =                     autoMove startLoc alarmLoc shipShortestPath actor myStatusMap myActorMap dungeonMap
  >>= \targetReached -> if targetReached
                          (setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap @! True)
                          (return False)

simulateHandlingWithObject :: RoomNumber MyObject RoomNumber RoomNumber RoomStatus MyActor (Shared MyRoomStatusMap) (Shared MyRoomInventoryMap) DungeonMap
                           -> Task Bool
simulateHandlingWithObject startLoc object objectLoc alarmLoc status actor myStatusMap myInventoryMap dungeonMap
  =                     addLog actor.userName "Started Simulation" ("From " <+++ startLoc <+++
                                                                    " to " <+++	alarmLoc <+++
                                                                    " via " <+++ objectLoc <+++
                                                                    " fetching " <+++ object)
  >>|                   autoMove startLoc objectLoc shipShortestPath actor myStatusMap myActorMap dungeonMap
  >>= \objectReached -> if objectReached (pickupObject objectLoc object actor myActorMap myInventoryMap
  >>= \actor ->                          autoMove objectLoc alarmLoc shipShortestPath actor myStatusMap myActorMap dungeonMap
  >>= \targetReached -> if targetReached (useObject alarmLoc object actor myActorMap
  >>= \(actor, used) -> if used          (setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap @! True)
                                         (return False))
                                         (return False))
                                         (return False)

findClosestObject :: RoomNumber (RoomNumber, RoomStatus) MyRoomStatusMap MyRoomInventoryMap RoomExitLockMap DungeonMap -> (Maybe RoomNumber, Maybe MyObject)
findClosestObject  myLoc (alarmLoc, status) statusMap inventoryMap exitLocks curMap
  | hasSmoke status = (Just myLoc, Nothing)
  | hasWater status = case findClosest myLoc alarmLoc Plug statusMap inventoryMap exitLocks curMap of
                        Nothing -> (Nothing, Nothing)
                        Just (obj, _, roomNo)  -> (Just roomNo, Just obj)
  | hasFire status
    # fireLoc    = findClosest myLoc alarmLoc FireExtinguisher statusMap inventoryMap exitLocks curMap
    # blanketLoc = findClosest myLoc alarmLoc FireBlanket      statusMap inventoryMap exitLocks curMap
    = case (fireLoc, blanketLoc) of
        (Just (obj, _, roomNo), Nothing) -> (Just roomNo, Just obj)
        (Nothing, Just (obj, _, roomNo)) -> (Just roomNo, Just obj)
        (Just (obj1, dist1, roomNo1), Just (obj2, dist2, roomNo2))
          | less dist1 dist2 -> (Just roomNo1, Just obj1)
          | otherwise        -> (Just roomNo2, Just obj2)
        _                    -> (Nothing, Nothing)
  | otherwise = (Nothing, Nothing)
  where
  less c1 c2
  | c1 >=0 && c2 >= 0 = c1<c2

findClosest :: RoomNumber RoomNumber ObjectType MyRoomStatusMap MyRoomInventoryMap RoomExitLockMap DungeonMap -> Maybe (MyObject, Int, RoomNumber)
findClosest myLoc targetLoc objectType statusMap inventoryMap exitLocks dungeonMap
  = case smartShipPathToClosestObject objectType myLoc targetLoc statusMap inventoryMap exitLocks dungeonMap of
      (Just obj, cost,_,_, (_, distance, Just path))
        = case reverse path of
            []    -> Just (obj, cost, myLoc)
            [x:_] -> Just (obj, cost, fromExit x)
      _ = Nothing

mkRoom :: !Network -> MyMkRoom
mkRoom network = \statusMap actorMap invMap exitLocks room -> viewInformation "Room Status" [imageView (\x -> (x, NoMapClick)) (\_ -> roomImage exitLocks invMap statusMap actorMap network True (Just room)) (\_ _ -> Nothing)] ()

