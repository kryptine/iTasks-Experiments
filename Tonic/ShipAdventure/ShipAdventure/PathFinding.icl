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
  cost detectors = 1 + sum (map detectorCost detectors)
  detectorCost (FireDetector  True) = 1000
  detectorCost (SmokeDetector True) = 250
  detectorCost (FloodDetector True) = 1000
  detectorCost _                    = 0

