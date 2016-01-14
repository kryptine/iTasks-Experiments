implementation module ShipAdventure.Util

import ShipAdventure.Types

isCarrying :: ObjectType MyActor -> Bool
isCarrying objType` {Actor | carrying } = objTypeInList objType` carrying

objTypeInList :: ObjectType [MyObject] -> Bool
objTypeInList objType` objs = length [0 \\ {Object | objType } <- objs | objType == objType`] > 0
