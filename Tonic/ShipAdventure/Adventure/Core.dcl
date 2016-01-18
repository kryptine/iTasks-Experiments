definition module Adventure.Core
 
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
from Data.IntMap.Strict import :: IntMap
import qualified Data.Map as DM
from Data.Map import :: Map
import GenLexOrd

:: DungeonMap :== [Floor]
:: Floor      :== [[Room]]
:: Room       =   { number :: RoomNumber // all room numbers should be unique !!
                  , name   :: String     // just a naam of the room for convenience of orientation
                  , exits  :: [Exit]     // room without doors (exits) make no sense
                  }

:: Object objType =
  { objId    :: Int
  , objType  :: objType
  , reusable :: Bool
  , portable :: Bool
  , quantity :: Int
  }

:: Locked     :== Bool
:: RoomNumber :== Int
:: Weight     :== Int
:: Distance   :== Int

:: Exit       = North RoomNumber
              | East  RoomNumber
              | South RoomNumber
              | West  RoomNumber
              | Up    RoomNumber
              | Down  RoomNumber

:: Actor obj act = { userName    :: User         // all actors should have unique names !!
                   , carrying    :: [Object obj] // can be anything
                   , actorStatus :: act          // can be anything
                   }

// the actor task will constantly be informed about the latest state of the actor, room, and map
// return True to stop the task
:: ActorTask r o a b :== (Actor o a) Room (RoomStatusMap r) (RoomActorMap o a) (RoomInventoryMap o) DungeonMap -> Task (Maybe b)

:: RoomStatusMap    roomStatus          :== IntMap roomStatus
:: RoomInventoryMap objType             :== IntMap [Object objType]
:: RoomActorMap     objType actorStatus :== IntMap [Actor objType actorStatus]
:: RoomExitLockMap                      :== Map (RoomNumber, Exit) Locked

instance == (Actor o a)
instance == (Object obj) | == obj
instance == Exit

derive gLexOrd Exit
instance < Exit

exitLockShare :: RWShared () RoomExitLockMap RoomExitLockMap

lockStatusForExit :: RWShared (RoomNumber, Exit) Locked Locked

mapLens :: String (RWShared () (Map a b) (Map a b)) (Maybe b) -> RWShared a b b | < a & == a

intMapLens :: String (RWShared () (IntMap a) (IntMap a)) (Maybe a) -> RWShared Int a a

fromExit :: Exit -> Int

derive class iTask Room, Exit, Actor, Object
instance toString (Object obj) | toString obj

// place an new actor into a room of your shared map after which the actor can freely move around

addActorToMap :: (Room -> Task ()) (Actor o a) RoomNumber (Shared (RoomStatusMap r))
                 (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o)) DungeonMap
              -> Task () | iTask r & iTask o & iTask a & Eq o

// move around the map until you return something

moveAround :: (Room -> Task ()) (Actor o a) (Maybe (ActorTask r o a b)) (Shared (RoomStatusMap r)) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o)) DungeonMap
           -> Task (Maybe b) | iTask r & iTask o & iTask a & Eq o & iTask b

// finds all actors currently walking on the map, find all objects in the map

findAllObjects :: (RoomInventoryMap o) -> [(RoomNumber, Object o)] | iTask o
findUser       :: User (RoomActorMap o a) -> Maybe (RoomNumber, Actor o a) | iTask o & iTask a

// update the status of an actor, unique username is used as identification

updActorStatus :: User (a -> a) (Shared (RoomActorMap o a)) -> Task () | iTask a & iTask o

getRoomFromMap 	:: RoomNumber DungeonMap -> Maybe Room

// update room status, unique room number is used as identification

updRoomStatus :: RoomNumber (r -> r) (Shared (RoomStatusMap r)) -> Task () | iTask r

toggleExit :: RoomNumber Exit DungeonMap -> Task ()
lockExit   :: RoomNumber Exit DungeonMap -> Task ()
unlockExit :: RoomNumber Exit DungeonMap -> Task ()

// shortest path calculation

shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(RoomStatusMap r) !RoomExitLockMap !DungeonMap
             -> Maybe ([Exit], Distance)

// auto movement from actors thru the map, fetching. dropping and using objects

autoMove :: RoomNumber RoomNumber
            (RoomNumber RoomNumber (RoomStatusMap r) RoomExitLockMap DungeonMap -> Maybe ([Exit], Distance))
            (Actor o a) (Shared (RoomStatusMap r)) (Shared (RoomActorMap o a)) DungeonMap
         -> Task Bool | iTask r & iTask o & iTask a & Eq o
pickupObject :: RoomNumber (Object o) (Actor o a) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o))
             -> Task Bool | iTask o & iTask a & Eq o
dropObject :: RoomNumber (Object o) (Actor o a) (Shared (RoomActorMap o a)) (Shared (RoomInventoryMap o))
           -> Task Bool | iTask o & iTask a & Eq o
useObject :: RoomNumber (Object o) (Actor o a) (Shared (RoomActorMap o a)) -> Task Bool | iTask o & iTask a & Eq o
getObjectOfType :: (Actor o a) o -> Object o | iTask o & iTask a

// given a shortest path algorithm, the current location and the kind of object one searches for
// returns: number of objects found, location of the closest object, distance to that object, shortest path to that object
//pathToClosestObject :: (RoomNumber !RoomNumber DungeonMap -> Maybe ([Exit], Distance)) o RoomNumber DungeonMap 
//															-> (Int, (RoomNumber, Distance, Maybe ([Exit], Distance))) | Eq o 
															
// given a shortest path algorithm, the current location, the kind of object one searches for, and the destination where the object has to be taken to
// returns: number of objects found, location of the closest object, distance to that object, shortest path to that object
smartPathToClosestObject :: (RoomNumber RoomNumber (RoomStatusMap r) RoomExitLockMap DungeonMap -> Maybe ([Exit], Distance))
                            o RoomNumber RoomNumber (RoomStatusMap r) (RoomInventoryMap o) RoomExitLockMap DungeonMap
                         -> (Maybe (Object o), Int, Distance, Int, (RoomNumber, Distance, Maybe [Exit])) | iTask o & iTask r & Eq o

