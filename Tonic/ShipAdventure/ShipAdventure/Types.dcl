definition module ShipAdventure.Types
 
import Adventure.Core
import GenLexOrd

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

:: ActorStatus	= 	{ occupied	:: Availability
					}
:: Availability	=	Available | NotAvailable | Busy  

:: Priority		=	Low | Normal | High | Highest

// logical devices

:: DeviceType =
  { kind		    :: 	DeviceKind
  , requires        :: 	Map CableType Capacity
  , produces        :: 	Map CableType Capacity
  }
:: DeviceKind		=	Radar
					| 	PowerGenerator
					| 	CoolingPump
					| 	Gun
:: CableType 		= 	PowerCable | CoolingPipe | DataCable
:: Capacity 		:== Int

// physical devices 

:: Network =
  { devices      	:: IntMap [Device]     					// [RoomNumber |-> Devices]
  , cables       	:: IntMap Cable        					// [CableId |-> Cable]
  , cableMapping 	:: IntMap [(Operational, RoomNumber)] 	// [CableId |-> RoomNumbers]
  }
:: Device =
  { description		::	String
  , deviceType		::	DeviceType
  , deviceId		::  DeviceId
  , inCables 		:: 	[CableId]
  , outCables 		:: 	[CableId]
  }
:: DeviceId			:== Int
:: CableId 			:== Int
:: Cable = 													// Edge
  { description 	:: String
  , cableId     	:: CableId
  , capacity    	:: Capacity
  , cableType   	:: CableType
  }
:: Operational :== Bool

derive gLexOrd CableType
derive class iTask Detector, ObjectType, ActorStatus, Availability, DeviceType
derive class iTask Cable, Priority, MapClick, Network, Device, CableType, DeviceKind

instance ==       ObjectType
instance ==       Priority

instance <  CableType
instance == CableType

instance toString ObjectType
instance toString Exit
instance toString Detector

// shared stores:

myMap          :: DungeonMap // map of the ship
myStatusMap    :: RWShared () MyRoomStatusMap    MyRoomStatusMap
myInventoryMap :: RWShared () MyRoomInventoryMap MyRoomInventoryMap
myActorMap     :: RWShared () MyRoomActorMap     MyRoomActorMap
myNetwork      :: RWShared () Network Network

statusInRoomShare    :: RWShared RoomNumber RoomStatus RoomStatus
inventoryInRoomShare :: RWShared RoomNumber (IntMap MyObject) (IntMap MyObject)
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

roomImage :: !RoomExitLockMap !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Network !Bool !(Maybe Room) !*TagSource -> Image (a, MapClick)

cutCable :: RoomNumber CableId Network -> Network

patchCable :: RoomNumber CableId Network -> Network
manageDevices :: Bool -> Task ()
