implementation module ShipAdventure.PathFinding

import Adventure.Core
import ShipAdventure.Types

// returns: distance, number of objects found, location of object, distance to object, shortest path to obejct
//shipPathToClosestObject :: Object RoomNumber MyMap -> (Int,(RoomNumber,Distance, Maybe ([Exit], Distance)))  
//shipPathToClosestObject kind actorLoc curMap = pathToClosestObject shipShortestPath kind actorLoc curMap

smartShipPathToClosestObject :: ObjectType RoomNumber RoomNumber MyRoomStatusMap MyRoomInventoryMap RoomExitLockMap DungeonMap -> (Maybe MyObject, Int,Distance, Int,(RoomNumber,Distance, Maybe [Exit])) 
smartShipPathToClosestObject kind actorLoc targetLoc statusMap inventoryMap exitLocks curMap = smartPathToClosestObject shipShortestPath kind actorLoc targetLoc statusMap inventoryMap exitLocks curMap


// shortest path given the alarms set on the ship

shipShortestPath :: RoomNumber RoomNumber MyRoomStatusMap RoomExitLockMap DungeonMap -> Maybe ([Exit], Distance)
shipShortestPath startRoomNumber endRoomNumber statusMap exitLocks allRooms = shortestPath cost startRoomNumber endRoomNumber statusMap exitLocks allRooms
  where
  cost status = 1 + statusCost status
  statusCost HasSomeWater  = 500
  statusCost IsFlooded     = 1000
  statusCost HasSmoke      = 400
  statusCost HasSmallFire  = 500
  statusCost HasMediumFire = 750
  statusCost HasBigFire    = 1000
  statusCost _             = 0

