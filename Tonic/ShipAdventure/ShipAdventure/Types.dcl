definition module ShipAdventure.Types
 
import Adventure.Core

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

myMap             	 :: RWShared () MyMap MyMap 			// map of the ship

allAvailableActors 	:: ReadOnlyShared [(RoomNumber, MyActor)]
allActors 		   	:: ReadOnlyShared [(RoomNumber, MyActor)]
allActiveAlarms 	:: ReadOnlyShared [(RoomNumber, Detector)]

// setting and resetting of the detection systems:

setAlarm 		:: User (RoomNumber,Detector) Bool (Shared MyMap) -> Task ()


isHigh 			:: !Detector -> Bool
updDetector 	:: !(Detector -> Detector) !Detector !RoomStatus -> RoomStatus
toggleDetector 	:: !Detector -> Detector
setDetector 	:: !Detector -> Detector
resetDetector 	:: !Detector -> Detector

// path finding:

// shortest path from room i to j

shipShortestPath  :: RoomNumber RoomNumber MyMap -> Maybe [Exit]


// making images from a map

showMap 			:: Task MapClick
setRoomDetectors 	:: Task ()

mapImage 	:: !Bool !(!MyMap, MapClick) !*TagSource -> Image (a, MapClick)
floorImage 	:: !Bool !(!MyFloor, !Int) !*TagSource -> *(!Image (a, MapClick), !*TagSource)
roomImage 	:: !Bool !(Maybe MyRoom) !*TagSource -> Image (a, MapClick)

