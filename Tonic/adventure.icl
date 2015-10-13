implementation module adventure
 
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

:: PreviousIdx :== Int
:: NodeIdx     :== Int
:: Distance :== Int
:: Weight      :== Int
:: Graph r o a :== IntMap (Distance, PreviousIdx, Room r o a)

shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(MAP r o a) -> [Exit]
shortestPath cost startRoomNumber endRoomNumber allRooms
  = reconstructSP (findSP cost (mkGraph allRooms) ('DH'.singleton (0, startRoomNumber))) endRoomNumber []
  where
  reconstructSP :: !(Graph r o a) !RoomNumber ![Exit] -> [Exit]
  reconstructSP graph currIdx path
    | currIdx == startRoomNumber = path
    | otherwise = case 'DIS'.get currIdx graph of
                    Just (_, prevIdx, _) -> case 'DIS'.get prevIdx graph of
                                              Just (_, _, {exits}) -> case [e \\ e <- exits | fromExit e == currIdx] of
                                                                        [] -> path
                                                                        [exit : _] -> reconstructSP graph prevIdx [exit : path]
                                              _                     -> path
                    _ -> path

  findSP :: !(r -> Weight) !(Graph r o a) !(Heap (Distance, NodeIdx)) -> Graph r o a
  findSP cost graph queue
    | 'DH'.null queue = graph
    | otherwise
      = case 'DH'.uncons queue of
          Just ((minDist, minIdx), queue)
            = case 'DIS'.get minIdx graph of
                Just (_, _, {exits})
                  #! (graph, queue) = foldr (\exit (graph, queue) -> case 'DIS'.get (fromExit exit) graph of
                                                                      Just (nDist, nPrevIdx, nRoom)
                                                                         #! alt = minDist + cost nRoom.roomStatus
                                                                        | alt < nDist
                                                                           = ( 'DIS'.alter (fmap (\(d, prev, r) -> (alt, minIdx, r))) nRoom.number graph
                                                                             , 'DH'.insert (alt, nRoom.number) queue)
                                                                        | otherwise = (graph, queue)) (graph, queue) exits
                  = findSP cost graph queue
                _ = graph
          _ = graph

  mkGraph :: !(MAP r o a) -> Graph r o a
  mkGraph playMap = foldr floorToGraph 'DIS'.newMap playMap
    where
    floorToGraph :: !(Floor r o a) !(Graph r o a) -> Graph r o a
    floorToGraph floor graph = foldr (\rooms graph -> foldr roomToGraph graph rooms) graph floor

    roomToGraph :: !(Room r o a) !(Graph r o a) -> Graph r o a
    roomToGraph room=:{number} graph
      #! dist = if (number == startRoomNumber) 0 67108864
    = 'DIS'.put number (dist, -1, room) graph

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East i) = i
fromExit (South i) = i
fromExit (West i) = i
fromExit (Up i) = i
fromExit (Down i) = i

// moving around in the map

addActorToMap :: (Actor o a) RoomNumber (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
addActorToMap actor location smap
	=			get smap
	>>= \map -> case (findUser actor.userName map) of
					Nothing	 	-> 			if (existsRoom location map)
									(		updateRoom location (entering actor) smap
									>>|		viewInformation ("You are in room " <+++ location <+++ ", now you can walk around") [] ()
									>>|		moveAround actor Nothing smap @! () 
									)(		viewInformation ("Room with number: " <+++ location <+++ " does not exist") [] () >>| return ()
									)
					Just (loc,me) ->		viewInformation ("You are already in room" <+++ loc) [] () >>| return ()

moveAround :: (Actor o a) (Maybe (ActorTask r o a)) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o
moveAround actor mbtask smap
	= 			repeatTask (\_ -> moveOneStep actor mbtask smap) id False

moveOneStep :: (Actor o a) (Maybe (ActorTask r o a)) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o
moveOneStep  actor mbtask smap
	= whileUnchanged smap
			(\map -> let room 	= findRoom actor map 
						 nactor = latestActorStatus actor room
					 in
					(	 (		(    viewInformation ("Hello " <+++ actor.userName <+++ ", you are in room " <+++ room.number) [] room
								 >>*    exitActions room nactor
                                     ++ inventoryActions room nactor
								     ++ carryActions room nactor
								)
								-||- 
								(if (isNothing mbtask) (viewInformation "-" [] () @! False ) ((fromJust mbtask) actor room map))  // Why a continue button ???
								
						)
			    )
			)
where
    exitActions room nactor
      = [ OnAction (Action ("Take Exit " <+++ exit) []) (always (move nactor room.number (fromExit exit) smap))
        \\ exit <- room.exits
        ]
    inventoryActions room nactor
      = [ OnAction (Action ("Fetch " <+++ object) []) (always (pickupObject nactor room object smap))
        \\ object <- room.inventory
        ]
    carryActions room nactor
      = [ OnAction (Action ("Drop " <+++ object) []) (always (dropDownObject nactor room object smap))
        \\ object <- nactor.carrying
        ]

	pickupObject actor room object smap
		=				updateRoom room.number (fetchObject object) smap
		>>|				return {actor & carrying = [object:actor.carrying]}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>|				return False
	
	dropDownObject actor room object smap
		=				updateRoom room.number (dropObject object) smap
		>>|				return {actor & carrying = removeMember object actor.carrying}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>|				return False

	move actor fromRoom toRoom smap
		= 				updateRoom fromRoom (leaving actor) smap
		>>| 			updateRoom toRoom (entering actor) smap
		>>|				return False


// perform a task given from outside

addTaskWhileWalking :: User User String String (ActorTask r o a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
addTaskWhileWalking fromUser forUser title priority task smap 
	=				get smap
	>>= \curMap ->	case findUser forUser curMap of
							Nothing 				-> return ()	
							Just (roomnumber,actor) -> appendTopLevelTaskPrioFor forUser title priority False 
														(		(			moveAround actor (Just task) smap 
																			-||-
																			(fromUser @: (viewInformation ("Stop process ") [] () >>|  return False))
																)
																>>= \b -> 	fromUser @: viewInformation ("Process " <+++ if b "terminated normally" "was killed") [] () >>| return ()
														) @! ()
														

// room updating

updateRoom :: RoomNumber ((Room r o a) -> (Room r o a)) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a
updateRoom roomNumber updRoom smap
	= 	upd (updateRoom` roomNumber updRoom) smap 
	>>| return ()
where 
	updateRoom` i upd [] 	  			= []
	updateRoom` i upd [floor:floors]   	= [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
	where
		updateThisRoom room = if (i == room.number) (upd room)  room

// actor status opdating

updActorStatus :: User (a -> a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
updActorStatus user upd smap 
	= 					get smap
		>>= \curMap ->	case findUser user curMap of
							Nothing 				-> return ()
							Just (roomnumber,actor) -> updateRoom roomnumber (updateActor (updStatus actor)) smap
where
	updStatus actor = {actor & actorStatus = upd actor.actorStatus}

// room status updating


getRoom :: RoomNumber (Shared (MAP r o a)) -> Task (Maybe (Room r o a)) | iTask r & iTask o & iTask a & Eq o
getRoom roomNumber smap = get smap >>= return o getRoomFromMap roomNumber

getRoomFromMap :: RoomNumber (MAP r o a) -> Maybe (Room r o a) | iTask r & iTask o & iTask a & Eq o
getRoomFromMap roomNumber m
	= case [room \\ room <- allRooms m | room.number == roomNumber] of
	  	[] -> Nothing
	  	status -> Just (hd status)

getRoomStatus :: RoomNumber (Shared (MAP r o a)) -> Task (Maybe r) | iTask r & iTask o & iTask a & Eq o
getRoomStatus roomNumber smap 
	=			get smap
	>>= \map ->	case [room.roomStatus \\ room <- allRooms map | room.number == roomNumber] of
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
findUser user map 
#	found = [(location,actor) \\ (location,actor) <- findAllActors map | actor.userName == user]
= if (isEmpty found) Nothing (Just (hd found)) 

findRoom :: (Actor o a) (MAP r o a) -> Room r o a
findRoom actor map 
# rooms	=	[ room
			\\ floor <- map, layer <- floor, room <- layer, {userName} <- room.actors
			| actor.userName == userName
			] 
= case rooms of 
	[]  -> abort "cannot find room of actor"
	_	-> hd rooms

latestActorStatus :: (Actor o a) (Room r o a) -> (Actor o a)
latestActorStatus actor room = hd [nactor \\ nactor <- room.actors | nactor.userName == actor.userName]

findAllActors :: (MAP r o a) ->  [(RoomNumber,(Actor o a))]
findAllActors map =	[ (room.number,actor)
					\\ floor <- map, layer <- floor, room <- layer, actor <- room.actors
					]

findAllObjects :: (MAP r o a) -> [(RoomNumber,o)]
findAllObjects map =	[ (room.number,object)
						\\ floor <- map, layer <- floor, room <- layer, object <- room.inventory
						]

allRoomStatus :: (MAP r o a) -> [(RoomNumber,r)] 
allRoomStatus map = [(number,roomStatus) \\ {number,roomStatus} <- allRooms map]


allRoomNumbers :: (MAP r o a) ->  [RoomNumber]
allRoomNumbers map = 	[room.number
						\\ floor <- map, layer <- floor, room <- layer
						]

allRooms :: (MAP r o a) ->  [Room r o a]
allRooms map = [room \\ floor <- map, layer <- floor, room <- layer]

existsRoom :: RoomNumber (MAP r o a) -> Bool
existsRoom i map = isMember i (allRoomNumbers map)

