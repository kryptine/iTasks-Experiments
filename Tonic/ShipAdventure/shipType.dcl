definition module shipType
 
import adventure

:: MyMap		:== MAP 	RoomStatus Object ActorStatus
:: MyActor		:== Actor 	Object ActorStatus
:: MyFloor		:== Floor 	RoomStatus Object ActorStatus
:: MyRoom		:== Room 	RoomStatus Object ActorStatus

:: MapClick     = NoMapClick
                | SelectRoom RoomNumber
                | ToggleAlarm RoomNumber Detector
                | ToggleDoor RoomNumber Exit

:: RoomStatus 	:==	[Detector] 
:: Detector		= 	FireDetector Bool 
				| 	SmokeDetector Bool
                | 	FloodDetector Bool
:: Object 		= 	FireExtinguisher
				| 	Blanket
				| 	Plug
:: ActorStatus	= 	{ occupied	:: Availability
					}
:: Availability	=	Available | NotAvailable | Busy  
:: Priority		=	Low | Normal | High | Highest

derive class iTask Detector, Object, ActorStatus, Availability, Priority, MapClick

instance 	== 			Object     
instance 	== 			Priority   

instance	toString 	Object 
instance 	toString 	Exit 
instance 	toString 	Detector
	
// shared stores:

myMap              :: Shared MyMap // map of the ship

allAvailableActors :: ReadOnlyShared [(RoomNumber, MyActor)]

allActiveAlarms :: ReadOnlyShared [(RoomNumber, Detector)]

// setting and resetting of the detection systems:

setAlarm 		:: User (RoomNumber,Detector) Bool (Shared MyMap) -> Task ()


isHigh 			:: !Detector -> Bool
updDetector 	:: !(Detector -> Detector) !Detector !RoomStatus -> RoomStatus
toggleDetector 	:: !Detector -> Detector
setDetector 	:: !Detector -> Detector
resetDetector 	:: !Detector -> Detector

// path finding:

// shortest path from room i to j

shipShortestPath  :: RoomNumber RoomNumber MyMap -> [Exit]

// returns: number of objects found, location of object, distance to object, shortest path to obejct

pathToClosestObject :: Object RoomNumber MyMap -> (Int,(RoomNumber,Int,[Exit])) 


// find room of closest detector located elsewhere ???

findClosestObject :: RoomNumber (RoomNumber,Detector) MyMap -> (Maybe RoomNumber,Maybe Object)

// making images from a map

showMap 			:: Task MapClick
setRoomDetectors 	:: Task ()

mapImage 	:: !Bool !(!MyMap, MapClick) !*TagSource -> Image (!MyMap, MapClick)
floorImage 	:: !Bool !(!MyFloor, !Int) !*TagSource -> *(!Image (!MyMap, MapClick), !*TagSource)
roomImage 	:: !Bool !(Maybe MyRoom) !*TagSource -> Image (!MyMap, !MapClick)

// the following shared stores are a temp fix, instead lenses should be used on myMap (did not have time for that)

alarmChanged 		:: Shared Bool			// to notify that one of the alarms has changed, should be turned into a lens
actorStatusChanged 	:: Shared Bool			// to notify that the status of one the actors has changed, should be tuned into a lens 

signalActorStatusChange	:: Task ()


