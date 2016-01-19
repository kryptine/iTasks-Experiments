definition module ShipAdventure.Types
 
import Adventure.Core

:: MyActor  :== Actor ObjectType ActorStatus
:: MyObject :== Object ObjectType

:: MyRoomStatusMap    :== RoomStatusMap RoomStatus
:: MyRoomInventoryMap :== RoomInventoryMap ObjectType
:: MyRoomActorMap     :== RoomActorMap ObjectType ActorStatus

:: MyMkRoom :== MkRoom RoomStatus ObjectType ActorStatus

:: MapClick     = NoMapClick
                | SelectRoom RoomNumber
                | ToggleAlarm RoomNumber Detector
                | ToggleDoor RoomNumber Exit

:: RoomStatus 	:==	[Detector] 
:: Detector		= 	FireDetector Bool 
				| 	SmokeDetector Bool
                | 	FloodDetector Bool


:: ObjectType	= 	FireExtinguisher
				| 	FireBlanket
				| 	Plug
				| 	Radar
				| 	PowerGen
				| 	CoolingPump

:: ActorStatus	= 	{ occupied	:: Availability
					}
:: Availability	=	Available | NotAvailable | Busy  
:: Priority		=	Low | Normal | High | Highest

// cableId and inRoom together form pri-key for Cable
:: Cable =
  { cableId    :: Int
  , inRoom     :: RoomNumber
  , connectsTo :: CableConnection
  }

:: CableConnection
  = RoomConn RoomNumber
  | DeviceConn Int

derive class iTask Detector, ObjectType, ActorStatus, Availability
derive class iTask Cable, CableConnection, Priority, MapClick

instance 	== 			ObjectType
instance	== 			Priority   

instance	toString 	ObjectType
instance 	toString 	Exit 
instance 	toString 	Detector
	
// shared stores:

myMap          :: DungeonMap // map of the ship
myStatusMap    :: RWShared () MyRoomStatusMap    MyRoomStatusMap
myInventoryMap :: RWShared () MyRoomInventoryMap MyRoomInventoryMap
myActorMap     :: RWShared () MyRoomActorMap     MyRoomActorMap

statusInRoomShare    :: RWShared RoomNumber RoomStatus RoomStatus
inventoryInRoomShare :: RWShared RoomNumber [MyObject] [MyObject]
actorsInRoomShare    :: RWShared RoomNumber [MyActor] [MyActor]

allActiveAlarms    :: ReadOnlyShared [(RoomNumber, Detector)]
allAvailableActors :: ReadOnlyShared [(RoomNumber, MyActor)]

// setting and resetting of the detection systems:

setAlarm 		:: User (RoomNumber,Detector) Bool (Shared MyRoomStatusMap) -> Task ()


isHigh 			:: !Detector -> Bool
updDetector 	:: !(Detector -> Detector) !Detector !RoomStatus -> RoomStatus
toggleDetector 	:: !Detector -> Detector
setDetector 	:: !Detector -> Detector
resetDetector 	:: !Detector -> Detector


// making images from a map

showMap 			:: Task MapClick
setRoomDetectors 	:: Task ()

roomImage :: !RoomExitLockMap !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Bool !(Maybe Room) !*TagSource -> Image (a, MapClick)

