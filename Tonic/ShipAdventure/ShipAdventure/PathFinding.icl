implementation module ShipAdventure.PathFinding

import Adventure.Core
import ShipAdventure.Types

shipPathToClosestObject :: Object RoomNumber MyMap -> (Int,(RoomNumber,Int,[Exit]))  // returns: number of objects found, location of object, distance to object, shortest path to obejct
shipPathToClosestObject kind actorLoc curMap = pathToClosestObject shipShortestPath kind actorLoc curMap

