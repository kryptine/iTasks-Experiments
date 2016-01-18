 implementation module Adventure.Core
 
import StdArray
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import qualified Data.Map as DM
from Data.Map import :: Map
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
import qualified Data.Heap as DH
from Data.Heap import :: Heap
import GenLexOrd
from Adventure.Logging import addLog
import Data.List

import StdMisc


derive class iTask Room, Exit, Actor, Object

// small utility functions 

instance == (Actor o a)  where (==) a1 a2 = a1.userName == a2.userName

instance == Exit where (==) e1 e2 =e1 === e2

derive gLexOrd Exit

instance < Exit where
  (<) e1 e2 = gLexOrd{|*|} e1 e2 === LT

instance == (Object obj) | == obj where
  (==) o1 o2 = o1 == o2

instance toString (Object obj) | toString obj where
  toString {Object | objId, objType} = toString objType +++ " " +++ toString objId

:: PreviousIdx :== Int
:: NodeIdx     :== Int
:: Weight      :== Int
:: Graph       :== IntMap (Distance, PreviousIdx, Room)

infinity =: 67108864

mapLens :: String (RWShared () (Map a b) (Map a b)) (Maybe b) -> RWShared a b b | < a & == a
mapLens name origShare mdef = sdsLens name (const ()) (SDSRead (read mdef)) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: (Maybe b) a (Map a b) -> MaybeError TaskException b | < a & == a
  read mdef idx m
    = case 'DM'.get idx m of
        Just x -> Ok x
        _      -> case mdef of
                    Just def -> Ok def
                    _        -> Error (exception (name +++ " (mapLens): Index not found"))

  write :: a (Map a b) b -> MaybeError TaskException (Maybe (Map a b)) | < a & == a
  write idx oldmap newval = Ok (Just ('DM'.put idx newval oldmap))

  notify :: a (Map a b) b -> SDSNotifyPred a | < a & == a
  notify idx oldmap newval = \idx` -> idx == idx`

intMapLens :: String (RWShared () (IntMap a) (IntMap a)) (Maybe a) -> RWShared Int a a
intMapLens name origShare mdef = sdsLens name (const ()) (SDSRead (read mdef)) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: (Maybe a) Int (IntMap a) -> MaybeError TaskException a
  read mdef idx intmap
    = case 'DIS'.get idx intmap of
        Just x -> Ok x
        _      -> case mdef of
                    Just def -> Ok def
                    _        -> Error (exception (name +++ " (intMapLens): Index " +++ toString idx +++ " not found"))

  write :: Int (IntMap a) a -> MaybeError TaskException (Maybe (IntMap a))
  write idx oldmap newval = Ok (Just ('DIS'.put idx newval oldmap))

  notify :: Int (IntMap a) a -> SDSNotifyPred Int
  notify idx oldmap newval = \idx` -> idx == idx`

exitLockShare :: RWShared () RoomExitLockMap RoomExitLockMap
exitLockShare = sharedStore "exitLockShare" 'DM'.newMap

lockStatusForExit :: RWShared (RoomNumber, Exit) Locked Locked
lockStatusForExit = mapLens "lockStatusForExit" exitLockShare (Just False)

shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(RoomStatusMap r) !RoomExitLockMap !DungeonMap
             -> Maybe ([Exit], Distance)
shortestPath cost startRoomNumber endRoomNumber statusMap exitLocks allRooms
  = reconstructSP (findSP cost (mkGraph allRooms) ('DH'.singleton (0, startRoomNumber)) statusMap)
  where
  reconstructSP :: !Graph -> Maybe ([Exit], Distance)
  reconstructSP graph
    = case 'DIS'.get endRoomNumber graph of
        Just (d, _, _) -> fmap (\x -> (x, d)) (reconstructSP` graph endRoomNumber [])
        _              -> Nothing

  reconstructSP` :: !Graph !RoomNumber ![Exit] -> Maybe [Exit]
  reconstructSP` graph currIdx path
    | currIdx == startRoomNumber = Just path
    | otherwise = case 'DIS'.get currIdx graph of
                    Just (_, prevIdx, _)
                      -> case 'DIS'.get prevIdx graph of
                           Just (_, _, {exits})
                             -> case [e \\ e <- exits | fromExit e == currIdx] of
                                  [] -> Nothing
                                  [exit : _] -> reconstructSP` graph prevIdx [exit : path]
                           _ -> Nothing
                    _ -> Nothing

  findSP :: !(r -> Weight) !Graph !(Heap (Distance, NodeIdx)) !(RoomStatusMap r) -> Graph
  findSP cost graph queue statusMap
    | 'DH'.null queue = graph
    | otherwise
      = case 'DH'.uncons queue of
          Just ((minDist, minIdx), queue)
            = case 'DIS'.get minIdx graph of
                Just (_, _, {exits})
                  #! (graph, queue) = foldr (foldExits statusMap cost minDist minIdx) (graph, queue) exits
                  = findSP cost graph queue statusMap
                _ = graph
          _ = graph
    where
    foldExits :: !(RoomStatusMap r) !(r -> Weight) !Distance !NodeIdx !Exit !(!Graph, !Heap (Distance, NodeIdx))
              -> (!Graph, !Heap (Distance, NodeIdx))
    foldExits statusMap cost minDist minIdx exit (graph, queue)
      | 'DM'.get (minIdx, exit) exitLocks == Just False
        #! exitNo = fromExit exit
        = case ('DIS'.get exitNo graph, 'DIS'.get exitNo statusMap) of
            (Just (nDist, nPrevIdx, nRoom), Just roomStatus)
              #! alt = minDist + cost roomStatus
              | alt < nDist
                = ( 'DIS'.alter (fmap (\(_, _, r) -> (alt, minIdx, r))) nRoom.number graph
                  , 'DH'.insert (alt, nRoom.number) queue)
              | otherwise = (graph, queue)
            _ = (graph, queue)
      | otherwise = (graph, queue)

  mkGraph :: !DungeonMap -> Graph
  mkGraph playMap = foldr floorToGraph 'DIS'.newMap playMap
    where
    floorToGraph :: !Floor !Graph -> Graph
    floorToGraph floor graph = foldr (\rooms graph -> foldr roomToGraph graph rooms) graph floor

    roomToGraph :: !Room !Graph -> Graph
    roomToGraph room=:{number} graph
      #! dist = if (number == startRoomNumber) 0 infinity
      = 'DIS'.put number (dist, -1, room) graph

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East  i) = i
fromExit (South i) = i
fromExit (West  i) = i
fromExit (Up    i) = i
fromExit (Down  i) = i

// moving around in the map

addActorToMap :: (Room -> Task ()) (Actor o a) RoomNumber (Shared (RoomStatusMap r))
                 (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o)) DungeonMap
              -> Task () | iTask r & iTask o & iTask a & Eq o
addActorToMap roomViz actor location shStatusMap shRoomActorMap shRoomInventoryMap dungeonMap
  | existsRoom location dungeonMap
    =   enterRoom actor location shRoomActorMap
    >>| moveAround roomViz actor noTask shStatusMap shRoomActorMap shRoomInventoryMap dungeonMap @! ()
  | otherwise = viewInformation ("Room with number: " <+++ location <+++ " does not exist") [] () >>| return ()
  where
  noTask :: Maybe (ActorTask r o a ()) | iTask o & iTask a & Eq o
  noTask = Nothing

moveAround :: (Room -> Task ()) (Actor o a) (Maybe (ActorTask r o a b))
              (Shared (RoomStatusMap r)) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o)) DungeonMap
           -> Task (Maybe b) | iTask r & iTask o & iTask a & Eq o & iTask b
moveAround roomViz actor mbtask shStatusMap shRoomActorMap shRoomInventoryMap dungeonMap
  = repeatTask (\_ -> moveOneStep roomViz actor mbtask shStatusMap shRoomActorMap shRoomInventoryMap dungeonMap) isJust Nothing

moveOneStep :: (Room -> Task ()) (Actor o a) (Maybe (ActorTask r o a b))
               (Shared (RoomStatusMap r)) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o)) DungeonMap
            -> Task (Maybe b) | iTask r & iTask o & iTask a & Eq o & iTask b
moveOneStep roomViz actor mbtask shStatusMap shRoomActorMap shRoomInventoryMap dungeonMap
  = whileUnchanged (shStatusMap |+| shRoomActorMap |+| shRoomInventoryMap |+| exitLockShare)
      (\(((statusMap, roomActorMap), roomInventory), exitLocks)
         -> case findActorRoom actor roomActorMap dungeonMap of
              Just room
                = ( roomViz room
                    >>* (  exitActions room actor exitLocks
                        ++ inventoryActions room roomInventory actor
                        ++ carryActions room actor
                        ) @! Nothing
                   ) -||-
                   (case mbtask of
                      Nothing -> viewInformation "" [] () @! Nothing
                      Just t  -> t actor room statusMap roomActorMap roomInventory dungeonMap
                   )
              _ = viewInformation "Failed to find actor" [] "Failed to find actor" @! Nothing
      )
  where
  exitActions room nactor exitLocks
    = [ OnAction (Action ("Go " <+++ exit) []) (always (move room.number (fromExit exit) nactor shRoomActorMap))
      \\ exit <- room.exits
      | 'DM'.get (room.number, exit) exitLocks == Just False
      ]
  inventoryActions room roomInventory nactor
    = case 'DIS'.get room.number roomInventory of
        Just objects
          = [ OnAction (Action ("Fetch " <+++ object) []) (always (pickupObject room.number object nactor shRoomActorMap shRoomInventoryMap))
            \\ object <- objects
            ]
        _ = []
  carryActions room nactor
    = [ OnAction (Action ("Drop " <+++ object) []) (always (dropObject room.number object nactor shRoomActorMap shRoomInventoryMap))
      \\ object <- nactor.carrying
      ]

pickupObject :: RoomNumber (Object o) (Actor o a) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o))
             -> Task Bool | iTask o & iTask a & Eq o
pickupObject roomNumber object actor shRoomActorMap shRoomInventoryMap
  =   updateRoomInventory roomNumber (\inv -> [obj \\ obj <- inv | obj.objId <> object.objId]) shRoomInventoryMap
  >>| updateActor roomNumber {actor & carrying = [object:actor.carrying]} shRoomActorMap
  >>| return True

dropObject :: RoomNumber (Object o) (Actor o a) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o))
           -> Task Bool | iTask o & iTask a & Eq o
dropObject roomNumber object actor shRoomActorMap shRoomInventoryMap
  =   updateRoomInventory roomNumber (\inv -> [object : inv]) shRoomInventoryMap
  >>| updateActor roomNumber {actor & carrying = removeMember object actor.carrying} shRoomActorMap
  >>| return True

move ::  RoomNumber RoomNumber (Actor o a) (Shared (RoomActorMap o a)) -> Task Bool | iTask o & iTask a & Eq o
move fromRoom toRoom actor shRoomActorMap
  =   leaveRoom actor fromRoom shRoomActorMap
  >>| enterRoom actor toRoom shRoomActorMap
  >>| return True

useObject :: RoomNumber (Object o) (Actor o a) (Shared (RoomActorMap o a)) -> Task Bool | iTask o & iTask a & Eq o
useObject roomNumber object actor shRoomActorMap
  =   updateActor roomNumber {actor & carrying = removeMember object actor.carrying} shRoomActorMap
  >>| return True

getObjectOfType :: (Actor o a) o -> Object o | iTask o & iTask a
getObjectOfType {Actor | carrying} objType` = case [obj \\ obj <- carrying | obj.objType === objType`] of
                                                [x : _] -> x

// auto moves around the maze

autoMove :: RoomNumber RoomNumber
            (RoomNumber RoomNumber (RoomStatusMap r) RoomExitLockMap DungeonMap -> Maybe ([Exit], Distance))
            (Actor o a) (Shared (RoomStatusMap r)) (Shared (RoomActorMap o a)) DungeonMap
         -> Task Bool | iTask r & iTask o & iTask a & Eq o
autoMove thisRoom target pathFun actor shStatusMap shActorMap dungeonMap
  | thisRoom == target = return True
  | otherwise
      =                 get shStatusMap
      >>= \statusMap -> get shActorMap
      >>= \actorMap  -> get exitLockShare
      >>= \exitLocks -> case findActorRoom actor actorMap dungeonMap of
                          Just room
                            # path = pathFun thisRoom target statusMap exitLocks dungeonMap
                            = case pathFun thisRoom target statusMap exitLocks dungeonMap of
                                Just (path=:[_:_], _)
                                  # nextRoom = fromExit (hd path)
                                  =     waitForTimer {Time | hour = 0, min = 0, sec = delay}
                                    >>| move room.number nextRoom actor shActorMap
                                    >>| addLog actor.userName "" ("Has moved to Room " <+++ nextRoom)
                                    >>| waitForTimer {Time | hour = 0, min = 0, sec = delay}
                                    >>| autoMove nextRoom target pathFun actor shStatusMap shActorMap dungeonMap
                                _ = return False
                          _ = return False

delay = 1

// room updating

updateActor :: RoomNumber (Actor o a) (Shared (RoomActorMap o a)) -> Task () | iTask o & iTask a
updateActor roomNumber actor smap
  = upd ('DIS'.alter (fmap (\actors -> [if (a.userName == actor.userName) actor a \\ a <- actors])) roomNumber) smap @! ()

// TODO Use shares with room-number focus domain
updateRoomInventory :: RoomNumber ([Object o] -> [Object o]) (Shared (RoomInventoryMap o))
                    -> Task () | iTask o
updateRoomInventory roomNumber updRoom smap
  =              get smap
  >>= \invMap -> case 'DIS'.get roomNumber invMap of
                   Just objs -> set ('DIS'.put roomNumber (updRoom objs) invMap) smap @! ()
                   _         -> return ()

// actor status opdating

updActorStatus :: User (a -> a) (Shared (RoomActorMap o a)) -> Task () | iTask a & iTask o
updActorStatus user upd smap
  =             get smap
  >>= \smap` -> case findUser user smap` of
                  Nothing                  -> return ()
                  Just (roomnumber, actor) -> updateActor roomnumber actor smap
  where
  updStatus actor = {actor & actorStatus = upd actor.actorStatus}

// room status updating

toggleExit :: RoomNumber Exit DungeonMap -> Task ()
toggleExit roomNo exit smap = updExit roomNo exit smap not

lockExit :: RoomNumber Exit DungeonMap -> Task ()
lockExit roomNo exit smap = updExit roomNo exit smap (const True)

unlockExit :: RoomNumber Exit DungeonMap -> Task ()
unlockExit roomNo exit smap = updExit roomNo exit smap (const False)

updExit :: RoomNumber Exit DungeonMap (Locked -> Locked) -> Task ()
updExit roomNo exit smap lockf = return () // TODO FIXME
  //= upd (map (map (map (updRoom (inverseExit exit))))) smap @! ()
  //where
  //updRoom :: !Exit !Room -> Room
  //updRoom ie r = r // TODO FIXME {r & exits = [if (interestingExit ie r e) (e, lockf l) (e, l)  \\ (e, l) <- r.exits] }

  //interestingExit :: !Exit !Room !Exit -> Bool
  //interestingExit ie r e = (r.number == roomNo && e == exit) || (r.number == fromExit exit && e == ie)

  //inverseExit :: !Exit -> Exit
  //inverseExit (North _) = South roomNo
  //inverseExit (South _) = North roomNo
  //inverseExit (East _)  = West roomNo
  //inverseExit (West _)  = East roomNo
  //inverseExit (Up _)    = Down roomNo
  //inverseExit (Down _)  = Up roomNo

getRoomFromMap :: RoomNumber DungeonMap -> Maybe Room
getRoomFromMap roomNumber m
  = case [room \\ room <- allRooms m | room.number == roomNumber] of
      []     -> Nothing
      status -> Just (hd status)

updRoomStatus :: RoomNumber (r -> r) (Shared (RoomStatusMap r)) -> Task () | iTask r
updRoomStatus roomNumber f smap
  = upd ('DIS'.alter (fmap f) roomNumber) smap @! ()

// room updating utility functions

leaveRoom :: (Actor o a) RoomNumber (Shared (RoomActorMap o a)) -> Task () | iTask o & iTask a
leaveRoom actor roomNo shStatusMap
  = upd ('DIS'.alter (fmap (\actors -> [a \\ a <- actors | a <> actor])) roomNo) shStatusMap @! ()

enterRoom :: (Actor o a) RoomNumber (Shared (RoomActorMap o a)) -> Task () | iTask o & iTask a
enterRoom actor roomNo shActorMap
  = upd f shActorMap @! ()
  where
  f actorMap
    # actors = 'DIS'.findWithDefault [] roomNo actorMap
    = 'DIS'.put roomNo (nub [actor:actors]) actorMap

// utility functions to find things located in the map

findActorRoom :: (Actor o a) (RoomActorMap o a) DungeonMap -> Maybe Room
findActorRoom actor rmap dungeonMap
  # roomNos = [ roomNo
              \\ (roomNo, actors) <- 'DIS'.toList rmap
              , actor` <- actors
              | actor.userName == actor`.userName
              ]
  = case roomNos of
      []           -> Nothing
      [roomNo : _] -> case findRoom roomNo dungeonMap of
                        Just r -> Just r
                        _      -> Nothing

findAllObjects :: (RoomInventoryMap o) -> [(RoomNumber, Object o)] | iTask o
findAllObjects objectMap = [ (roomNo, object)
                           \\ (roomNo, objects) <- 'DIS'.toList objectMap
                           , object <- objects
                           ]

// TODO Make this more efficient
findUser :: User (RoomActorMap o a) -> Maybe (RoomNumber, Actor o a) | iTask o & iTask a
findUser usr actorMap = case [ (roomNo, actor) \\ (roomNo, actors) <- 'DIS'.toList actorMap
                             , actor <- actors
                             | actor.userName == usr] of
                          [x : _] -> Just x
                          _       -> Nothing

findRoom :: RoomNumber DungeonMap -> Maybe Room
findRoom _ [] = Nothing
findRoom roomNo [floor : floors] = case findRoom` floor of
                                     Nothing -> findRoom roomNo floors
                                     x       -> x
  where
  findRoom` []               = Nothing
  findRoom` [rooms : roomss] = case findRoom`` rooms of
                                 Nothing -> findRoom` roomss
                                 x       -> x
  findRoom`` [] = Nothing
  findRoom`` [room : rooms]
    | room.number == roomNo = Just room
    | otherwise             = Nothing

allRooms :: DungeonMap -> [Room]
allRooms dungeonMap = [room \\ floor <- dungeonMap, layer <- floor, room <- layer]

existsRoom :: RoomNumber DungeonMap -> Bool
existsRoom i map = isMember i [  room.number
                              \\ floor <- map, layer <- floor, room <- layer
                              ]

// returns: number of objects found, location of object, distance to object, shortest path to obejct
//shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(RoomStatusMap r) !RoomExitLockMap !DungeonMap
             //-> Maybe ([Exit], Distance)

pathToClosestObject :: (RoomNumber RoomNumber (RoomStatusMap r) RoomExitLockMap DungeonMap -> Maybe ([Exit], Distance))
                       o RoomNumber (RoomStatusMap r) (RoomInventoryMap o) RoomExitLockMap DungeonMap
                    -> (Int, (RoomNumber, Distance, Maybe ([Exit], Distance))) | iTask o & iTask r & Eq o
pathToClosestObject sp kind actorLoc statusMap inventoryMap exitLocks dungeonMap
  # spath = sortBy (\(_, i, _) (_, j, _) -> i < j)
                   (filter (\(ol, l, p) -> ol >= 0)
                           [case sp actorLoc objectLoc statusMap exitLocks dungeonMap of
                              path=:(Just (_, dist)) -> (objectLoc, dist, path)
                              _                      -> (-1, infinity, Nothing)
                           \\ (objectLoc, found) <- findAllObjects inventoryMap | found.objType == kind ])
  = case spath of
      [x=:(_, _, Just (path, _)) :_] -> (length spath, x)
      []                             -> (-1, (-1, -1, Nothing))

// returns: number of objects found, location of object, distance to object, shortest path to obejct
smartPathToClosestObject :: (RoomNumber RoomNumber (RoomStatusMap r) RoomExitLockMap DungeonMap -> Maybe ([Exit], Distance))
                            o RoomNumber RoomNumber (RoomStatusMap r) (RoomInventoryMap o) RoomExitLockMap DungeonMap
                         -> (Maybe (Object o), Int, Distance, Int, (RoomNumber, Distance, Maybe [Exit])) | iTask o & iTask r & Eq o
smartPathToClosestObject spath objectKind actorLoc targetLoc statusMap inventoryMap exitLocks dungeonMap
  # foundObjects = [tpl \\ tpl=:(_, found) <- findAllObjects inventoryMap | found.objType == objectKind ]
  | isEmpty foundObjects = (Nothing, infinity, infinity, 0, (-1, -1, Nothing))
  # pathsFound = sortBy (\(_, i, _, _) (_, j, _, _) -> i < j)
                        (filter (\(_, d, _, (loc, dist, path)) -> isJust path)
                        [ let (oPath, oDistance) = case spath actorLoc objectLoc statusMap exitLocks dungeonMap of
                                                     (Just (path, distance)) -> (Just path, distance)
                                                     _                       -> (Nothing, infinity)
                              (tPath, tDistance) = case spath objectLoc targetLoc statusMap exitLocks dungeonMap of
                                                     (Just (path, distance)) -> (Just path, distance)
                                                     _                       -> (Nothing, infinity)
                              totalPathDist      = case (oPath, tPath) of
                                                     (Just xs, Just ys) -> length xs + length ys
                                                     _                  -> infinity
                          in (obj, oDistance + tDistance, totalPathDist, (objectLoc, oDistance, oPath))
                        \\ (objectLoc, obj) <- foundObjects | objectLoc <> targetLoc
                        ])
  = case pathsFound of
      [(obj, cost, totalDist, x=:(_, _, Just path)) :_] -> (Just obj, cost, totalDist, length pathsFound, x)
      []                                                -> (Nothing, infinity, infinity, -1, (-1, -1, Nothing))
