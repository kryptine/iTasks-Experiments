definition module ShipAdventure.PathFinding

import ShipAdventure.Types


// given object to search for, current location and current map
smartShipPathToClosestObject :: ObjectType RoomNumber RoomNumber MyRoomStatusMap MyRoomInventoryMap RoomExitLockMap DungeonMap
                             -> (Maybe MyObject,Int,Distance, Int,(RoomNumber,Distance, Maybe [Exit])) 

// given object to search for, current location, target room to move to with object, and current map
shipShortestPath :: RoomNumber RoomNumber MyRoomStatusMap RoomExitLockMap DungeonMap
                 -> Maybe ([Exit], Distance)
