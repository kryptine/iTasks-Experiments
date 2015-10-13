definition module adventure
 
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
from Data.IntMap.Strict import :: IntMap

:: MAP r o a	:== [Floor r o a]
:: Floor r o a	:==	[[Room r o a]]
:: Room roomStatus object actorStatus
				=	{ name		 	:: String						// just a naam of the room for convenience of orientation
					, number 	 	:: RoomNumber					// all room numbers should be unique !!	
					, exits		 	:: [Exit]						// room without doors (exits) make no sense
					, roomStatus	:: roomStatus					// can be anything
					, inventory	 	:: [object]						// can be anyting
					, actors	 	:: [Actor object actorStatus]	// actors are users who can freely move around the map
					}
:: RoomNumber	:== Int
:: Weight       :== Int
:: Exit			=	North Int
				|	East Int
				|	South Int
				|	West Int
				|	Up Int
				|	Down Int
:: Actor o a	=	{ userName		:: User							// all actors should have unique names !!
					, carrying		:: [o]							// can be anything
					, actorStatus	:: a							// can be anything
					}

:: ActorTask r o a	:== (MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)

instance == (Actor o a)  

derive class iTask Room, Exit, Actor

// place an new actor into a room of your shared map after which the actor can move around
// a task to do needs to be give to the actor

addActorToMap :: (Actor o a) RoomNumber (ActorTask r o a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o

// finds all actors currently walking on the map

findAllActors :: (MAP r o a) ->  [(RoomNumber,(Actor o a))] 

// update the status of an actor, unique username is used as identification

updActorStatus :: User (a -> a) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o

// collect roomnumbers / roomstatus from your map

allRoomNumbers 	:: (MAP r o a) ->  [RoomNumber]
allRoomStatus 	:: (MAP r o a) ->  [(RoomNumber,r)]

getRoom :: RoomNumber (Shared (MAP r o a)) -> Task (Maybe (Room r o a)) | iTask r & iTask o & iTask a & Eq o
getRoomStatus 	:: RoomNumber (Shared (MAP r o a)) -> Task (Maybe r) | iTask r & iTask o & iTask a & Eq o

// update room status, unique room number is used as identification

updRoomStatus :: RoomNumber (r -> r) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o


shortestPath :: !(r -> Weight) !RoomNumber !RoomNumber !(MAP r o a) -> [Exit]
