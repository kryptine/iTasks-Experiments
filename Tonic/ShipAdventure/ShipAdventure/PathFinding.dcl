definition module ShipAdventure.PathFinding

import ShipAdventure.Types

shipPathToClosestObject :: Object RoomNumber MyMap -> (Distance,Int,(RoomNumber,Distance, Maybe ([Exit], Distance)))  // returns: number of objects found, location of object, distance to object, shortest path to obejct

shipShortestPath :: RoomNumber RoomNumber MyMap -> Maybe ([Exit], Distance)
