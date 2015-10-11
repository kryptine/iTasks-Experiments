implementation module adventure
 
import StdArray
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

import StdMisc

:: MAP r o a	:== [Floor r o a]
:: Floor r o a	:==	[[Room r o a]]
:: Room roomStatus object actorStatus
				=	{ name		 	:: String
					, number 	 	:: RoomNumber
					, exits		 	:: [Exit]
					, roomStatus	:: roomStatus
					, inventory	 	:: [object]
					, actors	 	:: [Actor object actorStatus]
					}
:: RoomNumber	:== Int 
:: Exit			=	North Int
				|	East Int
				|	South Int
				|	West Int
				|	Up Int
				|	Down Int
:: Actor o a	=	{ userName		:: User
					, carrying		:: [o]
					, actorStatus	:: a
					}

:: ActorTask r o a	:== (MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)

derive class iTask Room, Exit, Actor

// small utility functions 

instance == (Actor o a)  where (==) a1 a2 = a1.userName == a2.userName

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East i) = i
fromExit (South i) = i
fromExit (West i) = i
fromExit (Up i) = i
fromExit (Down i) = i

// moving around in the map

addActorToMap :: (Actor o a) RoomNumber (ActorTask r o a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
addActorToMap actor location task smap
	=			get smap
	>>= \map -> case (findUser actor.userName map) of
					Nothing	 	-> 			if (existsRoom location map)
									(		updateRoom location (entering actor) smap
									>>|		viewInformation ("You are in room " <+++ location <+++ ", now you can walk around") [] ()
									>>|		moveAround actor task smap 
									)(		viewInformation ("Room with number: " <+++ location <+++ " does not exist") [] () >>| return ()
									)
					Just (loc,me) ->		viewInformation ("You are already in room" <+++ loc) [] () >>| return ()

moveAround :: (Actor o a) ((MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
moveAround actor task smap
	= forever (moveOnStep actor task smap)

moveOnStep :: (Actor o a) ((MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
moveOnStep  actor task smap
	= whileUnchanged smap
			(\map -> let room 	= findRoom actor map 
						 nactor = latestActorStatus actor room
					 in
					(	 (		(		viewInformation ("Hello " <+++ actor.userName <+++ ", you are in room " <+++ room.number) [] room
								 >>*	[ OnAction (Action ("Take Exit " <+++ exit) []) (always (move nactor room.number (fromExit exit) smap))
										\\ exit <- room.exits
										]
										++ 
										[ OnAction (Action ("Fetch " <+++ object) [])  	(always (pickupObject nactor room object smap))
										\\ object <- room.inventory
										]
										++
										[ OnAction (Action ("Drop " <+++ object) [])  	(always (dropDownObject nactor room object smap))
										\\ object <- nactor.carrying
										]
								)
								-||-
								(				task map room nactor
								>>= \actor ->	updateRoom room.number (updateActor actor) smap
								>>|				return ()
								)
 
						)
			    )
			)
where
	pickupObject actor room object smap
		=				updateRoom room.number (fetchObject object) smap
		>>|				return {actor & carrying = [object:actor.carrying]}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>|				return ()
	
	dropDownObject actor room object smap
		=				updateRoom room.number (dropObject object) smap
		>>|				return {actor & carrying = removeMember object actor.carrying}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>|				return ()

	move actor fromRoom toRoom smap
		= 				updateRoom fromRoom (leaving actor) smap
		>>| 			updateRoom toRoom (entering actor) smap
		>>|				return ()

// room updating

updateRoom :: RoomNumber ((Room r o a)-> (Room r o a)) (Shared (MAP r o a))-> Task () | iTask r & iTask o & iTask a
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

findRoom :: (Actor o a) (MAP r o a) -> (Room r o a)
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

