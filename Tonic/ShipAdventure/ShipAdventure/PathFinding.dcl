definition module ShipAdventure.PathFinding

import ShipAdventure.Types


// given object to search for, current location and current map
// returns: distance, number of objects found, location of object, distance to object, shortest path to object

//shipPathToClosestObject :: Object RoomNumber MyMap -> (Int,(RoomNumber,Distance, Maybe ([Exit], Distance)))  

// given object to search for, current location, target room to move to with object, and current map
// returns: cost, number of objects found, location of object, distance to object, shortest path to object

smartShipPathToClosestObject :: Object RoomNumber RoomNumber MyMap -> (Int,Int,(RoomNumber,Distance, Maybe [Exit])) 

shipShortestPath :: RoomNumber RoomNumber MyMap -> Maybe ([Exit], Distance)
