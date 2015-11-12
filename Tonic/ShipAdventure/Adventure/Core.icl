implementation module Adventure.Core
 
import StdArray
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
import qualified Data.Heap as DH
from Data.Heap import :: Heap

import StdMisc


derive class iTask Room, Exit, Actor

// small utility functions 

instance == (Actor o a)  where (==) a1 a2 = a1.userName == a2.userName

instance == Exit where (==) e1 e2 =e1 === e2

:: PreviousIdx :== Int
:: NodeIdx     :== Int
:: Weight      :== Int
:: Graph r o a :== IntMap (Distance, PreviousIdx, Room r o a)

infinity =: 67108864

shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(MAP r o a) -> Maybe ([Exit], Distance)
shortestPath cost startRoomNumber endRoomNumber allRooms
  = reconstructSP (findSP cost (mkGraph allRooms) ('DH'.singleton (0, startRoomNumber))) endRoomNumber []
  where
  reconstructSP :: !(Graph r o a) !RoomNumber ![Exit] -> Maybe ([Exit], Distance)
  reconstructSP graph currIdx path
    = case 'DIS'.get currIdx graph of
        Just (d, _, _) -> fmap (\x -> (x, d)) (reconstructSP` graph currIdx path)
        _              -> Nothing

  reconstructSP` :: !(Graph r o a) !RoomNumber ![Exit] -> Maybe [Exit]
  reconstructSP` graph currIdx path
    | currIdx == startRoomNumber = Just path
    | otherwise = case 'DIS'.get currIdx graph of
                    Just (_, prevIdx, _)
                      -> case 'DIS'.get prevIdx graph of
                           Just (_, _, {exits})
                             -> case [e \\ (e, _) <- exits | fromExit e == currIdx] of
                                  [] -> Nothing
                                  [exit : _] -> reconstructSP` graph prevIdx [exit : path]
                           _ -> Nothing
                    _ -> Nothing

  findSP :: !(r -> Weight) !(Graph r o a) !(Heap (Distance, NodeIdx)) -> Graph r o a
  findSP cost graph queue
    | 'DH'.null queue = graph
    | otherwise
      = case 'DH'.uncons queue of
          Just ((minDist, minIdx), queue)
            = case 'DIS'.get minIdx graph of
                Just (_, _, {exits})
                  #! (graph, queue) = foldr (foldExits cost minDist minIdx) (graph, queue) exits
                  = findSP cost graph queue
                _ = graph
          _ = graph
    where
    foldExits :: !(r -> Weight) !Distance !NodeIdx !(!Exit, !Locked) !(!Graph r o a, !Heap (Distance, NodeIdx)) -> (!Graph r o a, !Heap (Distance, NodeIdx))
    foldExits cost minDist minIdx (exit, False) (graph, queue)
      = case 'DIS'.get (fromExit exit) graph of
          Just (nDist, nPrevIdx, nRoom)
            #! alt = minDist + cost nRoom.roomStatus
            | alt < nDist
              = ( 'DIS'.alter (fmap (\(_, _, r) -> (alt, minIdx, r))) nRoom.number graph
                , 'DH'.insert (alt, nRoom.number) queue)
            | otherwise = (graph, queue)
          _ = (graph, queue)
    foldExits _ _ _ _ (graph, queue) = (graph, queue)

  mkGraph :: !(MAP r o a) -> Graph r o a
  mkGraph playMap = foldr floorToGraph 'DIS'.newMap playMap
    where
    floorToGraph :: !(Floor r o a) !(Graph r o a) -> Graph r o a
    floorToGraph floor graph = foldr (\rooms graph -> foldr roomToGraph graph rooms) graph floor

    roomToGraph :: !(Room r o a) !(Graph r o a) -> Graph r o a
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

addActorToMap :: ((Room r o a) -> Task ()) (Actor o a) RoomNumber (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
addActorToMap roomViz actor location smap
  =           get smap
  >>= \map -> if (existsRoom location map)
                (   updateRoom location (entering actor) smap
                >>| moveAround roomViz actor noTask smap @! ()
                )
                (viewInformation ("Room with number: " <+++ location <+++ " does not exist") [] () >>| return ())
  where
  noTask :: Maybe (ActorTask r o a ()) | iTask r & iTask o & iTask a & Eq o
  noTask = Nothing

moveAround :: ((Room r o a) -> Task ()) (Actor o a) (Maybe (ActorTask r o a b)) (Shared (MAP r o a)) -> Task (Maybe b) | iTask r & iTask o & iTask a & Eq o & iTask b
moveAround roomViz actor mbtask smap
  = repeatTask (\_ -> moveOneStep roomViz actor mbtask smap) isJust Nothing

moveOneStep :: ((Room r o a) -> Task ()) (Actor o a) (Maybe (ActorTask r o a b)) (Shared (MAP r o a)) -> Task (Maybe b) | iTask r & iTask o & iTask a & Eq o & iTask b
moveOneStep roomViz actor mbtask smap
	= whileUnchanged smap
			(\map -> let room 	= findRoom actor map 
						 nactor = latestActorStatus actor room
					 in
					(	 (		( roomViz room
								 >>* (   exitActions room nactor
                                      ++ inventoryActions room nactor
								      ++ carryActions room nactor
								     ) @! Nothing
								)
								-||- 
								(if (isNothing mbtask) (viewInformation "" [] () @!  Nothing) ((fromJust mbtask) nactor room map))  
						)
			    )
			)
where
    exitActions room nactor
      = [ OnAction (Action ("Go " <+++ exit) []) (always (move room.number (fromExit exit) nactor smap))
        \\ (exit, False) <- room.exits
        ]
    inventoryActions room nactor
      = [ OnAction (Action ("Fetch " <+++ object) []) (always (pickupObject room.number object nactor smap))
        \\ object <- room.inventory
        ]
    carryActions room nactor
      = [ OnAction (Action ("Drop " <+++ object) []) (always (dropDownObject room.number object nactor smap))
        \\ object <- nactor.carrying
        ]

pickupObject :: RoomNumber o (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 
pickupObject roomNumber object actor smap
	=		updateRoom roomNumber (fetchObject object) smap
	>>|		updateRoom roomNumber (updateActor {actor & carrying = [object:actor.carrying]}) smap
	>>|		return True

dropDownObject :: RoomNumber o (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 
dropDownObject roomNumber object actor smap
	=		updateRoom roomNumber (dropObject object) smap
	>>|		updateRoom roomNumber (updateActor {actor & carrying = removeMember object actor.carrying}) smap
	>>|		return True

move ::  RoomNumber RoomNumber (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 
move fromRoom toRoom actor smap
	= 		updateRoom fromRoom (leaving actor) smap
	>>| 	updateRoom toRoom (entering actor) smap
	>>|		return True

useObject :: RoomNumber o (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 
useObject roomNumber object actor smap
	=		updateRoom roomNumber (updateActor {actor & carrying = removeMember object actor.carrying}) smap
	>>|		return True

// auto moves around the maze

autoMove :: RoomNumber RoomNumber (RoomNumber RoomNumber (MAP r o a) -> Maybe ([Exit], Distance)) (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o
autoMove thisRoom target pathFun actor smap
| thisRoom == target 
	= 	return True
= 			   		get smap
	>>= \curMap -> 	let room 	= findRoom actor curMap 
				 		nactor  = latestActorStatus actor room
				 		path	= pathFun thisRoom target curMap
					in	case pathFun thisRoom target curMap of
                          Just (path=:[_:_], _)
						    # nextRoom = fromExit (hd path)
							=     waitForTimer  {Time | hour = 0, min = 0, sec = delay}
							  >>| move room.number nextRoom nactor smap
					 		  >>| waitForTimer  {Time | hour = 0, min = 0, sec = delay} 
					 		  >>| autoMove nextRoom target pathFun nactor smap
                          _ = return False

delay = 1

// room updating

updateRoom :: RoomNumber ((Room r o a) -> (Room r o a)) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a
updateRoom roomNumber updRoom smap
  =   upd (updateRoom` roomNumber updRoom) smap
  >>| return ()
  where
  updateRoom` i upd []             = []
  updateRoom` i upd [floor:floors] = [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
    where
    updateThisRoom room
    | i == room.number = upd room
    | otherwise        = room

// actor status opdating

updActorStatus :: User (a -> a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
updActorStatus user upd smap
  =              get smap
  >>= \curMap -> case findUser user curMap of
                   Nothing                  -> return ()
                   Just (roomnumber, actor) -> updateRoom roomnumber (updateActor (updStatus actor)) smap
  where
  updStatus actor = {actor & actorStatus = upd actor.actorStatus}

// room status updating

toggleExit :: RoomNumber Exit (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
toggleExit roomNo exit smap = updExit roomNo exit smap not

lockExit :: RoomNumber Exit (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
lockExit roomNo exit smap = updExit roomNo exit smap (const True)

unlockExit :: RoomNumber Exit (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
unlockExit roomNo exit smap = updExit roomNo exit smap (const False)

updExit :: RoomNumber Exit (Shared (MAP r o a)) (Locked -> Locked) -> Task () | iTask r & iTask o & iTask a & Eq o
updExit roomNo exit smap lockf
  = upd (map (map (map (updRoom (inverseExit exit))))) smap @! ()
  where
  updRoom :: !Exit !(Room r o a) -> Room r o a | iTask r & iTask o & iTask a & Eq o
  updRoom ie r = {r & exits = [if (interestingExit ie r e) (e, lockf l) (e, l)  \\ (e, l) <- r.exits] }

  interestingExit :: !Exit !(Room r o a) !Exit -> Bool
  interestingExit ie r e = (r.number == roomNo && e == exit) || (r.number == fromExit exit && e == ie)

  inverseExit :: !Exit -> Exit
  inverseExit (North _) = South roomNo
  inverseExit (South _) = North roomNo
  inverseExit (East _)  = West roomNo
  inverseExit (West _)  = East roomNo
  inverseExit (Up _)    = Down roomNo
  inverseExit (Down _)  = Up roomNo

getRoom :: RoomNumber (Shared (MAP r o a)) -> Task (Maybe (Room r o a)) | iTask r & iTask o & iTask a & Eq o
getRoom roomNumber smap = get smap >>= return o getRoomFromMap roomNumber

getRoomFromMap :: RoomNumber (MAP r o a) -> Maybe (Room r o a) | iTask r & iTask o & iTask a & Eq o
getRoomFromMap roomNumber m
  = case [room \\ room <- allRooms m | room.number == roomNumber] of
      []     -> Nothing
      status -> Just (hd status)

getRoomStatus :: RoomNumber (Shared (MAP r o a)) -> Task (Maybe r) | iTask r & iTask o & iTask a & Eq o
getRoomStatus roomNumber smap
  =           get smap
  >>= \map -> case [room.roomStatus \\ room <- allRooms map | room.number == roomNumber] of
                [] -> return Nothing
                status -> return (Just (hd status))

updRoomStatus :: RoomNumber (r -> r) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
updRoomStatus roomNumber upd smap = updateRoom roomNumber (\room -> {room & roomStatus = upd room.roomStatus}) smap

// room updating utility functions

leaving :: (Actor o a) (Room r o a) -> (Room r o a) | Eq o
leaving actor room = {room & actors = removeMember actor room.actors}

entering :: (Actor o a) (Room r o a) -> (Room r o a)
entering actor room = {room & actors = [actor:room.actors]}

fetchObject :: o (Room r o a) -> (Room r o a) | Eq o
fetchObject object room = {room & inventory = removeMember object room.inventory}

dropObject ::  o (Room r o a) -> (Room r o a)
dropObject object room = {room & inventory = [object:room.inventory]}

updateActor :: (Actor o a) (Room r o a) -> (Room r o a) | Eq o
updateActor actor room = {room & actors = [actor:removeMember actor room.actors]}

// utility functions to find things located in the map

findUser :: User (MAP r o a) ->  Maybe (RoomNumber,(Actor o a))
findUser user map = listToMaybe [ (location,actor) \\ (location,actor) <- findAllActors map
                                | actor.userName == user]

findRoom :: (Actor o a) (MAP r o a) -> Room r o a
findRoom actor map
  # rooms = [ room
            \\ floor <- map, layer <- floor, room <- layer, {userName} <- room.actors
            | actor.userName == userName
            ]
  = case rooms of
      []    -> abort "cannot find room of actor"
      [x:_] -> x

latestActorStatus :: (Actor o a) (Room r o a) -> Actor o a
latestActorStatus actor room = hd [nactor \\ nactor <- room.actors
                                  | nactor.userName == actor.userName]

findAllActors :: (MAP r o a) ->  [(RoomNumber, Actor o a)]
findAllActors map = [ (room.number,actor)
                    \\ floor <- map, layer <- floor, room <- layer, actor <- room.actors
                    ]

findAllObjects :: (MAP r o a) -> [(RoomNumber, o)]
findAllObjects map = [ (room.number,object)
                     \\ floor <- map, layer <- floor, room <- layer, object <- room.inventory
                     ]

findObjectsInRoom :: RoomNumber (MAP r o a) -> Maybe o
findObjectsInRoom roomNumber map = listToMaybe [o \\ (i,o) <- findAllObjects map | i == roomNumber]

allRoomStatus :: (MAP r o a) -> [(RoomNumber, r)]
allRoomStatus map = [(number,roomStatus) \\ {number,roomStatus} <- allRooms map]

allRoomNumbers :: (MAP r o a) ->  [RoomNumber]
allRoomNumbers map = [room.number
                     \\ floor <- map, layer <- floor, room <- layer
                     ]

allRooms :: (MAP r o a) ->  [Room r o a]
allRooms map = [room \\ floor <- map, layer <- floor, room <- layer]

existsRoom :: RoomNumber (MAP r o a) -> Bool
existsRoom i map = isMember i (allRoomNumbers map)

pathToClosestObject :: (RoomNumber !RoomNumber (MAP r o a) -> Maybe ([Exit], Distance)) o RoomNumber (MAP r o a) -> (Int, (RoomNumber, Int, Maybe ([Exit], Distance))) | Eq o // returns: number of objects found, location of object, distance to object, shortest path to obejct
pathToClosestObject sp kind actorLoc curMap
  # spath = sortBy (\(_, i, _) (_, j, _) -> i < j) [case sp actorLoc objectLoc curMap of
                                                      path=:(Just (_, dist)) -> (objectLoc, dist, path)
                                                      _                      -> (-1, infinity, Nothing)
                                                   \\ (objectLoc, found) <- findAllObjects curMap | found == kind ]
  = case spath of
      [x=:(_, _, Just (path, _)) :_] -> (length path, x)
      []                             -> (infinity, (-1, -1, Nothing))
