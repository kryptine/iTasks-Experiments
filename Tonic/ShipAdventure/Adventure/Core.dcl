definition module Adventure.Core
 
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
from Data.IntMap.Strict import :: IntMap

:: MAP r o a   :== [Floor r o a]
:: Floor r o a :== [[Room r o a]]
:: Room roomStatus object actorStatus
               =   { name       :: String                     // just a naam of the room for convenience of orientation
                   , number     :: RoomNumber                 // all room numbers should be unique !!  
                   , exits      :: [(Exit, Locked)]           // room without doors (exits) make no sense
                   , roomStatus :: roomStatus                 // can be anything
                   , inventory  :: [object]                   // can be anyting
                   , actors     :: [Actor object actorStatus] // actors are users who can freely move around the map
                   }
:: Locked      :== Bool
:: RoomNumber  :== Int
:: Weight      :== Int
:: Distance    :== Int
:: Exit        = North Int
               | East Int
               | South Int
               | West Int
               | Up Int
               | Down Int
:: Actor o a   = { userName    :: User // all actors should have unique names !!
                 , carrying    :: [o]  // can be anything
                 , actorStatus :: a    // can be anything
                 }

instance == (Actor o a)
instance == Exit

fromExit :: Exit -> Int

derive class iTask Room, Exit, Actor

// place an new actor into a room of your shared map after which the actor can freely move around

addActorToMap :: ((Room r o a) -> Task ()) (Actor o a) RoomNumber (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o

// move around the map until you return something

moveAround :: ((Room r o a) -> Task ()) (Actor o a) (Maybe (ActorTask r o a b)) (Shared (MAP r o a)) -> Task (Maybe b) | iTask r & iTask o & iTask a & Eq o & iTask b

// the actor task will constantly be informed about the latest state of the actor, room, and map
// return True to stop the task

:: ActorTask r o a b :== (Actor o a) (Room r o a) (MAP r o a) -> Task (Maybe b)

// finds all actors currently walking on the map, find all objects in the map

findAllActors  	:: (MAP r o a) -> [(RoomNumber,(Actor o a))] 
findAllObjects 	:: (MAP r o a) -> [(RoomNumber,o)]
findUser 		:: User (MAP r o a) ->  Maybe (RoomNumber,(Actor o a))

// update the status of an actor, unique username is used as identification

updActorStatus :: User (a -> a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o

// collect roomnumbers / roomstatus from your map

allRoomNumbers 	:: (MAP r o a) ->  [RoomNumber]
allRoomStatus 	:: (MAP r o a) ->  [(RoomNumber,r)]

getRoom 		:: RoomNumber (Shared (MAP r o a)) -> Task (Maybe (Room r o a)) | iTask r & iTask o & iTask a & Eq o
getRoomStatus 	:: RoomNumber (Shared (MAP r o a)) -> Task (Maybe r) | iTask r & iTask o & iTask a & Eq o
getRoomFromMap 	:: RoomNumber (MAP r o a) -> Maybe (Room r o a) | iTask r & iTask o & iTask a & Eq o

// update room status, unique room number is used as identification

updRoomStatus :: RoomNumber (r -> r) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o

toggleExit :: RoomNumber Exit (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
lockExit 	:: RoomNumber Exit (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
unlockExit 	:: RoomNumber Exit (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o

// shortest path calculation

shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(MAP r o a) -> Maybe ([Exit], Distance)

// auto movement from actors thru the map, fetching. dropping and using objects

autoMove 	   	:: RoomNumber RoomNumber (RoomNumber RoomNumber (MAP r o a) -> Maybe ([Exit], Distance)) (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o
pickupObject   	:: RoomNumber o (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 
useObject 	   	:: RoomNumber o (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 
dropDownObject 	:: RoomNumber o (Actor o a) (Shared (MAP r o a)) -> Task Bool | iTask r & iTask o & iTask a & Eq o 

// given a shortest path algorithm, the current location and the kind of object one searches for
// returns: number of objects found, location of the closest object, distance to that object, shortest path to that object
//pathToClosestObject :: (RoomNumber !RoomNumber (MAP r o a) -> Maybe ([Exit], Distance)) o RoomNumber (MAP r o a) 
//															-> (Int, (RoomNumber, Distance, Maybe ([Exit], Distance))) | Eq o 
															
// given a shortest path algorithm, the current location, the kind of object one searches for, and the destination where the object has to be taken to
// returns: number of objects found, location of the closest object, distance to that object, shortest path to that object
smartPathToClosestObject :: (RoomNumber !RoomNumber (MAP r o a) -> Maybe ([Exit], Distance)) o RoomNumber RoomNumber (MAP r o a) 
															-> (Distance, Int, (RoomNumber, Distance, Maybe [Exit])) | Eq o 

