implementation module ShipAdventure.Types
 
//import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data
import qualified Data.IntMap.Strict as DIS
import qualified Data.Map as DM

import Adventure.Core
from Adventure.Logging import addLog
import ShipAdventure.PathFinding

derive gLexOrd CableType
derive class iTask Detector, ObjectType, ActorStatus, Availability, DeviceType
derive class iTask Cable, Priority, MapClick, Network, Device, CableType, DeviceKind

// std overloading instances

//instance == Object      where (==) o1 o2 = o1 === o2
instance == ObjectType  where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2
instance toString ObjectType where
  toString FireExtinguisher = "Extinguiser"
  toString FireBlanket      = "Blanket"
  toString Plug             = "Plug"

instance toString Exit where toString exit = toSingleLineText exit

instance toString Detector
where toString (FireDetector _)  = "Fire Alarm"
	  toString (SmokeDetector _) = "Smoke Alarm"
	  toString (FloodDetector _) = "Flood Alarm"

instance toString Device
where toString {Device | description} = description

instance < CableType where
  (<) l r = case l =?= r of
              LT -> True
              _  -> False

instance == CableType where
  (==) l r = l === r

// shared stores:


myStatusMap :: RWShared () MyRoomStatusMap MyRoomStatusMap
myStatusMap = sharedStore "myStatusMap" 'DIS'.newMap

statusInRoomShare :: RWShared RoomNumber RoomStatus RoomStatus
statusInRoomShare = intMapLens "statusInRoomShare" myStatusMap (Just [])

myInventoryMap :: RWShared () MyRoomInventoryMap MyRoomInventoryMap
myInventoryMap = sharedStore "myInventoryMap" ('DIS'.fromList invs)
  where
  invs = [ (4,  'DIS'.fromList [ (1,  {Object | objId = 1,  objType = FireExtinguisher})
                               ])
         , (7,  'DIS'.fromList [ (2,  {Object | objId = 2,  objType = FireBlanket })
                               ])
         , (8,  'DIS'.fromList [ (3,  {Object | objId = 3,  objType = FireExtinguisher })])
         , (9,  'DIS'.fromList [ (4,  {Object | objId = 4,  objType = FireBlanket } )
                               ])
         , (10, 'DIS'.fromList [ (5,  {Object | objId = 5,  objType = FireExtinguisher })])
         , (14, 'DIS'.fromList [ (6,  {Object | objId = 6,  objType = FireExtinguisher })])
         , (17, 'DIS'.fromList [ (7,  {Object | objId = 7,  objType = FireBlanket })
                               , (8,  {Object | objId = 8,  objType = Plug })
                               ])
         , (19, 'DIS'.fromList [ (9,  {Object | objId = 9,  objType = FireBlanket })
                               , (10, {Object | objId = 10, objType = Plug })
                               ])
         , (20, 'DIS'.fromList [ (11, {Object | objId = 11, objType = FireExtinguisher})])
         ]

manageDevices :: Bool -> Task ()
manageDevices kitchen
  | kitchen
  =               get myNetwork
  >>= \network -> viewDevices
  >>*             [  OnAction (Action (if enabled "/Cut " "/Patch " +++ mkCableDesc roomNo cable) []) (always (if enabled (cutCableTask roomNo cable) (patchCableTask roomNo cable)))
                  \\ cable  <- 'DIS'.elems network.cables
                  ,  (enabled, roomNo) <- fromMaybe [] ('DIS'.get cable.cableId network.cableMapping)
                  ]
  | otherwise = viewDevices
  where
  viewDevices = (viewSharedInformation "Disabled devices" [ViewWith disabledDevices] myNetwork
                 -&&-
                 viewSharedInformation "Device network" [ViewWith ppNetwork] myNetwork
                ) @! ()

  ppNetwork network = (devicesView, cablesView)
    where
    cablesView = ("Cables", 'DIS'.elems network.cables)
    devicesView = ( "Devices"
                  , [  ("Room " +++ toString roomNo, map toPPDevice devices`)
                    \\ (roomNo, devices`) <- 'DIS'.toList network.devices
                    ]
                  )
  disabledDevices network = [  map toPPDevice (devicesForCable cable network)
                            \\ cable <- 'DIS'.elems network.cables
                            |  not (isOperational cable.cableId network.cableMapping)
                            ]
    where
    devicesForCable :: Cable Network -> [Device]
    devicesForCable cable=:{cableId} {cableMapping, devices}
      = [  device
        \\ (_, roomNo) <- fromMaybe [] ('DIS'.get cableId cableMapping)
        ,  device      <- fromMaybe [] ('DIS'.get roomNo devices)
        |  requiresCable cable device
        ]
      where
      // TODO Take resource quantity and redundancy into account
      requiresCable { Cable | cableType} device = isJust ('DM'.get cableType device.Device.deviceType.DeviceType.requires)

  mkCableDesc roomNo {Cable | cableId, description}
    = "'" +++ description +++ " " +++ toString cableId +++ "' in room " +++ toString roomNo
  cutCableTask roomNo cable
    = upd (cutCable roomNo cable.cableId) myNetwork >>| manageDevices kitchen
  patchCableTask roomNo cable
    = upd (patchCable roomNo cable.cableId) myNetwork >>| manageDevices kitchen

:: PPDevice =
  { description :: String
  , deviceType  :: PPDeviceType
  , deviceId    :: DeviceId
  , inCables    :: [CableId]
  , outCables   :: [CableId]
  }

:: PPDeviceType =
  { kind     :: DeviceKind
  , requires :: [(CableType, Capacity)]
  , produces :: [(CableType, Capacity)]
  }

derive class iTask PPDevice, PPDeviceType

toPPDevice :: Device -> PPDevice
toPPDevice { Device | description, deviceType, deviceId, inCables, outCables } = { PPDevice
                                                                                 | description = description
                                                                                 , deviceType  = toPPDeviceType deviceType
                                                                                 , deviceId    = deviceId
                                                                                 , inCables    = inCables
                                                                                 , outCables   = outCables
                                                                                 }

toPPDeviceType :: DeviceType -> PPDeviceType
toPPDeviceType { DeviceType | kind, requires, produces } = { PPDeviceType
                                                           | kind     = kind
                                                           , requires = 'DM'.toList requires
                                                           , produces = 'DM'.toList produces
                                                           }

isOperational cableId cableMapping = and [b \\ (b, _) <- fromMaybe [] ('DIS'.get cableId cableMapping)]

// my logical devices

radarDevice :: DeviceType
radarDevice    = { DeviceType
                 | kind     = Radar
                 , requires = 'DM'.fromList [(PowerCable, 1)]
                 , produces = 'DM'.fromList []
                 }
powerGenerator :: DeviceType
powerGenerator = { DeviceType
                 | kind     = PowerGenerator
                 , requires = 'DM'.fromList [] // TODO Cooling
                 , produces = 'DM'.fromList [(PowerCable, 10)]
                 }
gun :: DeviceType
gun            = { DeviceType
                 | kind     = Gun
                 , requires = 'DM'.fromList [(PowerCable, 1)]
                 , produces = 'DM'.fromList []
                 }

// my physical mapping of the devices in a network

myNetwork :: RWShared () Network Network
myNetwork = sharedStore "myNetwork"
  { Network
  | devices = 'DIS'.fromList [ (1, [{ Device // Radar
                                    | description	= "Radar"
                                    , deviceType	= radarDevice
                                    , deviceId		= 42
                                    , inCables		= [1]
                                    , outCables		= []
                                    }
                                   ])
                             , (5, [{ Device // Power gen
                                    | description	= "Main Power Generator"
                                    , deviceType	= powerGenerator
                                    , deviceId      = 24
                                    , inCables		= []
                                    , outCables		= [1, 2]
                                     }
                                   ])
                             , (9, [{ Device // Gun
                                    | description	= "Forward Gun"
                                    , deviceType	= gun
                                    , deviceId      = 64
                                    , inCables 		= [2]
                                    , outCables		= []
                                    }
                                   ])
                             ]
	,cables = 'DIS'.fromList [ (1, { Cable
                                  | cableId     = 1
                                  , description = "Radar power cable"
                                  , capacity    = 1
                                  , cableType   = PowerCable
                                  })
                            , (2, { Cable
                                  | cableId     = 2
                                  , description = "Forward Gun power cable"
                                  , capacity    = 1
                                  , cableType   = PowerCable
                                  })
                            ]
  , cableMapping = 'DIS'.fromList [ (1, [ (True, 1)
                                        , (True, 3)
                                        , (True, 5)
                                        ])
                                  , (2, [ (True, 5)
                                        , (True, 7)
                                        , (True, 9)
                                        ])
                                  ]
  }

cutCable :: RoomNumber CableId Network -> Network
cutCable roomNo cableId network = { network & cableMapping = 'DIS'.alter (fmap (\xs -> [(if (no == roomNo) False op, no) \\ (op, no) <- xs])) cableId network.cableMapping }

patchCable :: RoomNumber CableId Network -> Network
patchCable roomNo cableId network = { network & cableMapping = 'DIS'.alter (fmap (\xs -> [(if (no == roomNo) True op, no) \\ (op, no) <- xs])) cableId network.cableMapping }

inventoryInRoomShare :: RWShared RoomNumber (IntMap MyObject) (IntMap MyObject)
inventoryInRoomShare = intMapLens "inventoryInRoomShare" myInventoryMap (Just 'DIS'.newMap)

myActorMap :: RWShared () MyRoomActorMap MyRoomActorMap
myActorMap = sharedStore "myActorMap" 'DIS'.newMap

actorsInRoomShare :: RWShared RoomNumber [MyActor] [MyActor]
actorsInRoomShare = intMapLens "actorInRoomShare" myActorMap (Just [])

allAvailableActors :: ReadOnlyShared [(RoomNumber, MyActor)]
allAvailableActors
  = toReadOnly (sdsProject (SDSLensRead readActors) SDSNoWrite myActorMap)
  where
  readActors curMap = Ok [ (room, actor) \\ (room,actors) <- 'DIS'.toList curMap
                         , actor <- actors
                         | actor.actorStatus.occupied === Available]

allActiveAlarms :: ReadOnlyShared [(RoomNumber, Detector)]
allActiveAlarms
  = toReadOnly (sdsProject (SDSLensRead readAlarms) SDSNoWrite myStatusMap)
where
    readAlarms statusMap = Ok [ (number,detector) \\ (number,detectors) <- 'DIS'.toList statusMap
                              , detector <- detectors
                              | isHigh detector]

// detectors setting

// setting and resetting of the detection systems

setRoomDetectors :: Task ()
setRoomDetectors 
	= ((updateInformationWithShared "Map Status" [imageUpdate id (mapImage True myMap) (\_ _ -> Nothing) (const snd)] (exitLockShare |+| myInventoryMap |+| myStatusMap |+| myActorMap |+| myNetwork) NoMapClick
      >>* [OnValue (\tv -> case tv of
                             Value (ToggleAlarm selRoom d)   _ -> Just (updRoomStatus selRoom (updDetector toggleDetector d) myStatusMap >>| setRoomDetectors)
                             Value (ToggleDoor selRoom exit) _ -> Just (toggleExit selRoom exit myMap >>| setRoomDetectors)
                             _ -> Nothing
                   )])
      -||
      manageDevices True <<@ ArrangeHorizontal) <<@ ArrangeHorizontal <<@ FullScreen

setAlarm :: User (RoomNumber,Detector) Bool (Shared MyRoomStatusMap) -> Task ()
setAlarm user (alarmLoc,detector) bool shStatusMap
	= 		updRoomStatus alarmLoc (updDetector (if bool setDetector resetDetector) detector) shStatusMap
	>>|		addLog user ""  ("Resets " <+++ detector <+++ " in Room " <+++ alarmLoc <+++ " to False.") 

//

updDetector :: !(Detector -> Detector) !Detector !RoomStatus -> RoomStatus
updDetector f d r = [if (d =+?= d`) (f d`) d` \\ d` <- r]

toggleDetector :: !Detector -> Detector
toggleDetector (FireDetector  b) = FireDetector  (not b)
toggleDetector (SmokeDetector b) = SmokeDetector (not b)
toggleDetector (FloodDetector b) = FloodDetector (not b)

setDetector :: !Detector -> Detector
setDetector (FireDetector  b) = FireDetector  True
setDetector (SmokeDetector b) = SmokeDetector True
setDetector (FloodDetector b) = FloodDetector True

resetDetector :: !Detector -> Detector
resetDetector (FireDetector  b) = FireDetector  False
resetDetector (SmokeDetector b) = SmokeDetector False
resetDetector (FloodDetector b) = FloodDetector False

// general map viewing

showMap :: Task MapClick
showMap = (((updateInformationWithShared "Map Status" [imageUpdate id (mapImage False myMap) (\_ _ -> Nothing) (const snd)] (exitLockShare |+| myInventoryMap |+| myStatusMap |+| myActorMap |+| myNetwork) NoMapClick
             >&> withSelection (return ())
                   (\mapClick -> case mapClick of
                                   SelectRoom selRoom -> updateInformationWithShared "Room Status" [imageUpdate (\(((((exitLocks, invMap), statusMap), actorMap), network), a) -> ((exitLocks, invMap, statusMap, actorMap, network, getRoomFromMap selRoom myMap), a)) (\((exitLocks, invMap, statusMap, actorMap, network, room), _) -> roomImage exitLocks invMap statusMap actorMap network True room) (\_ _ -> Nothing) (const snd)] (exitLockShare |+| myInventoryMap |+| myStatusMap |+| myActorMap |+| myNetwork) NoMapClick
                                   _ -> return NoMapClick)) <<@ ArrangeHorizontal)
          -||
          manageDevices True <<@ ArrangeHorizontal) <<@ ArrangeHorizontal <<@ FullScreen

myMap :: DungeonMap // map of the ship
myMap = [floor0, floor1]
  where
  floor0    = [ [back0]
              , [room01, room02, room03]
              , [corridor0]
              , [room04, room05, room06]
              , [room07, room08]
              ]

  back0     = {name = "back 0",     number = 1,  exits = [South 2, South 3, South 4]}
  room01    = {name = "room 0.1",   number = 2,  exits = [North 1, South 5, Down 12]}
  room02    = {name = "room 0.2",   number = 3,  exits = [North 1, South 5]}
  room03    = {name = "room 0.3",   number = 4,  exits = [North 1, South 5]}
  corridor0 = {name = "corridor 0", number = 5,  exits = [ North 2, North 3, North 4
                                                         , South 6, South 7, South 8
                                                         ]}

  room04    = {name = "room 0.4",   number = 6,  exits = [North 5, South 9]}
  room05    = {name = "room 0.5",   number = 7,  exits = [North 5, South 9]}
  room06    = {name = "room 0.6",   number = 8,  exits = [North 5, South 10]}
  room07    = {name = "room 0.7",   number = 9,  exits = [North 6, North 7]}
  room08    = {name = "room 0.8",   number = 10, exits = [North 8, Down 20]}

  floor1    = [ [back1]
              , [room11, room12, room13]
              , [corridor1]
              , [room14, room15, room16]
              , [room17, room18]
              ]

  back1     = {name = "back 1",     number = 11, exits = [South 12, South 13, South 14]}
  room11    = {name = "room 1.1",   number = 12, exits = [North 11, South 15, Up 2]}
  room12    = {name = "room 1.2",   number = 13, exits = [North 11, South 15, East 14]}
  room13    = {name = "room 1.3",   number = 14, exits = [North 11, South 15, West 13]}
  corridor1 = {name = "corridor 1", number = 15, exits = [ North 12, North 13, North 14
                                                         , South 16, South 17, South 18
                                                         ]}

  room14    = {name = "room 1.4",   number = 16, exits = [North 15, South 19]}
  room15    = {name = "room 1.5",   number = 17, exits = [North 15, South 19]}
  room16    = {name = "room 1.6",   number = 18, exits = [North 15, South 20]}
  room17    = {name = "room 1.7",   number = 19, exits = [North 16, North 17]}
  room18    = {name = "room 1.8",   number = 20, exits = [North 18, Up 10]}

// making an image from the map ...
mapImage :: !Bool !DungeonMap !(!(!(!(!(!RoomExitLockMap, !MyRoomInventoryMap), !MyRoomStatusMap), !MyRoomActorMap), !Network), MapClick) !*TagSource -> Image (a, MapClick)
mapImage mngmnt m (((((exitLocks, inventoryMap), statusMap), actorMap), network), _) tsrc
  #! (floors, tsrc) = mapSt (floorImage exitLocks inventoryMap statusMap actorMap network mngmnt) (zip2 m (reverse [0..length m])) tsrc
  #! allFloors      = beside (repeat AtMiddleY) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) floors) Nothing
  #! legendElems    = [ (mkStatusBadgeBackground (FireDetector True),  "Fire detected")
                      , (mkStatusBadgeBackground (SmokeDetector True), "Smoke detected")
                      , (mkStatusBadgeBackground (FloodDetector True), "Flood detected")
                      , (mkActorBadgeBackground Available,             "Available person")
                      , (mkActorBadgeBackground NotAvailable,          "Unavailable person")
                      , (mkActorBadgeBackground Busy,                  "Busy person")
                      , (mkInventoryBadgeBackground True,              "Portable object")
                      , (mkInventoryBadgeBackground False,             "Non-portable object")
                      , (mkUpDown 0 (Up 0) 'DM'.newMap,                "Staircase up")
                      , (mkUpDown 0 (Down 0) 'DM'.newMap,              "Staircase down")
                      ]
  #! legendElems    = map (\(img, descr) -> beside (repeat AtMiddleY) [] [img, text myFontDef (" " +++ descr)] Nothing) legendElems
  #! legend         = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) legendElems) Nothing
  = beside [] [] [allFloors, empty (px 8.0) (px 8.0), legend] Nothing

floorImage :: !RoomExitLockMap !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Network !Bool !(!Floor, !Int) !*TagSource -> *(!Image (a, MapClick), !*TagSource)
floorImage exitLocks inventoryMap statusMap actorMap network mngmnt (floor, floorNo) [(floorTag, uFloorTag) : tsrc]
  #! (rooms, tsrc) = mapSt f floor tsrc
  #! floor         = tag uFloorTag (above (repeat AtMiddleX) [] [text myFontDef ("Deck " +++ toString floorNo) : rooms] Nothing)
  = (floor, tsrc)
  where
  f :: ![Room] !*TagSource -> *(!Image (a, MapClick), !*TagSource)
  f rooms tsrc
    #! (rooms`, tsrc) = mapSt (roomImage` inventoryMap statusMap actorMap network mngmnt False exitLocks) rooms tsrc
    = (beside (repeat AtMiddleY) [] rooms` Nothing, tsrc)

roomDim =: 64.0
exitWidth =: 16.0

myFontDef = normalFontDef "Arial" 10.0

roomImage :: !RoomExitLockMap !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Network !Bool !(Maybe Room) !*TagSource -> Image (a, MapClick)
roomImage exitLocks inventoryMap statusMap actorMap network zoomed (Just room) tsrc = fst (roomImage` inventoryMap statusMap actorMap network False zoomed exitLocks room tsrc)
roomImage _ _ _ _ _ _ _ _                                                           = empty zero zero

roomImage` :: !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Network !Bool !Bool !RoomExitLockMap !Room !*TagSource -> *(!Image (a, MapClick), !*TagSource)
roomImage` inventoryMap statusMap actorMap network mngmnt zoomed exitLocks room=:{number, exits} tsrc
  #! (northEs, eastEs, southEs, westEs, upEs, downEs) = foldr foldExit ([], [], [], [], [], []) exits
  #! numNorth        = length northEs
  #! numSouth        = length southEs
  #! numEast         = length eastEs
  #! numWest         = length westEs
  #! widthMul        = toReal (max (max numNorth numSouth) 1)
  #! heightMul       = toReal (max (max (length eastEs) (length westEs)) 1)
  #! multiplier      = if zoomed 2.0 1.0
  #! bgWidth         = multiplier * roomDim * widthMul
  #! bgHeight        = multiplier * roomDim * heightMul
  #! bg              = rect (px bgWidth) (px bgHeight) <@< { fill = toSVGColor "white" }
  #! bg              = bg <@< { onclick = onClick (SelectRoom number), local = False }
  #! roomStatus      = case 'DIS'.get number statusMap of
                         Just roomStatus = roomStatus
                         _
                         | mngmnt    = [FireDetector False, SmokeDetector False, FloodDetector False] // TODO This isnt really correct yet. When we get Just, other buttons will disappear. We need to fill the shares to make this all work.
                         | otherwise = []
  #! statusBadges    = above (repeat AtMiddleX) [] (foldr (mkStatusBadge number mngmnt multiplier) [] roomStatus) Nothing
  #! actors          = case 'DIS'.get number actorMap of
                         Just actors -> actors
                         _           -> []
  #! actorBadges     = above (repeat AtMiddleX) [] (map (scale multiplier multiplier o mkActorBadge) actors) Nothing
  #! inventory       = case 'DIS'.get number inventoryMap of
                         Just inv -> 'DIS'.elems inv
                         _        -> []
  #! devices         = fromMaybe [] ('DIS'.get number network.devices)
  #! inventoryBadges = map (\i -> scale multiplier multiplier (mkInventoryBadge True (toString i % (0, 1)))) inventory
  #! deviceBadges    = map (\i -> scale multiplier multiplier (mkInventoryBadge False (toString i % (0, 1)))) devices
  #! allBadges       = inventoryBadges ++ deviceBadges
  #! inventoryBadges = if (length allBadges > 0)
                         (beside (repeat AtMiddleY) [] allBadges Nothing)
                         (empty zero zero)
  #! roomNo          = text myFontDef (toString number) <@< { onclick = onClick (SelectRoom number), local = False }
  #! upDownExits     = above (repeat AtMiddleX) [] (map (\e -> scale multiplier multiplier (mkUpDown number e exitLocks) <@< { onclick = onClick (ToggleDoor number e), local = False }) (upEs ++ downEs)) Nothing
  #! (topExitAligns, topExitOffsets, topExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numNorth (AtLeft, AtTop)    northEs
  #! (botExitAligns, botExitOffsets, botExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numSouth (AtLeft, AtBottom) southEs
  #! (rExitAligns,   rExitOffsets,   rExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numEast  (AtRight, AtTop)   eastEs
  #! (lExitAligns,   lExitOffsets,   lExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numWest  (AtLeft, AtTop)    westEs
  #! total          = overlay ([(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtMiddleY), (AtLeft, AtBottom), (AtRight, AtBottom)] ++ topExitAligns ++ botExitAligns ++ rExitAligns ++ lExitAligns)
                              ([(px 3.0, px 3.0), (px -3.0, px 3.0), (zero, zero), (px 3.0, px -3.0), (px -3.0, px -3.0)] ++ topExitOffsets ++ botExitOffsets ++ rExitOffsets ++ lExitOffsets)
                              ([statusBadges, actorBadges, roomNo, inventoryBadges, upDownExits] ++ topExitImgs ++ botExitImgs ++ rExitImgs ++ lExitImgs) (Just bg)
  = (total, tsrc)
  where
  foldExit :: !Exit !(![Exit], ![Exit], ![Exit], ![Exit], ![Exit], ![Exit]) -> (![Exit], ![Exit], ![Exit], ![Exit], ![Exit], ![Exit])
  foldExit e=:(North _) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([e : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit e=:(East _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [e : eastEs], southEs, westEs, upEs, downEs)
  foldExit e=:(South _) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [e : southEs], westEs, upEs, downEs)
  foldExit e=:(West _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [e : westEs], upEs, downEs)
  foldExit e=:(Up _)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [e : upEs], downEs)
  foldExit e=:(Down _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [e : downEs])

  mkAsOsIs :: !Real !(Span -> (!Span, !Span)) !(Image (a, MapClick)) !Real !Int !(!XAlign, !YAlign) ![Exit]
            -> (![(!XAlign, !YAlign)], ![(!Span, !Span)], [Image (a, MapClick)])
  mkAsOsIs multiplier mkTuple doorImg bgSize num align es
    #! exitAligns  = repeatn num align
    #! exitOffsets = reverse (fst (foldr (\_ (xs, n) -> ([mkTuple (px n) : xs], n + roomDim * multiplier)) ([], (roomDim * multiplier - exitWidth) / 2.0) es))
    #! exitImgs    = map mkDoor es
    = (exitAligns, exitOffsets, exitImgs)
    where
    mkDoor exit
      #! locked = case 'DM'.get (number, exit) exitLocks of
                    Just b -> b
                    _      -> False
      = doorImg
          <@< { stroke = toSVGColor "black" }
          <@< { strokewidth = px 1.0 }
          <@< { fill = toSVGColor (if locked "black" "white") }
          <@< { onclick = onClick (ToggleDoor number exit), local = False }

//onClick :: !MapClick Int (a, MapClick) -> (a, MapClick)
onClick clck _ (m, _) = (m, clck)

mkUpDown :: !RoomNumber !Exit !RoomExitLockMap -> Image (a, MapClick)
mkUpDown roomNo e exitLocks
  # l = case 'DM'.get (roomNo, e) exitLocks of
          Just x -> x
          _      -> False
  # ps = case e of
           (Up _) -> [(px 0.0, px 0.0), (px 12.0, px -12.0), (px 12.0, px 0.0)]
           _      -> [(px 0.0, px -12.0), (px 12.0, px 0.0), (px 0.0, px 0.0)]
  = polygon Nothing ps <@< { opacity = if l 0.3 1.0 }

mkStatusBadge :: Int !Bool !Real !Detector ![Image (a, MapClick)] -> [Image (a, MapClick)]
mkStatusBadge roomNo mngmnt badgeMult d acc
  #! high = isHigh d
  | high || mngmnt
    #! img = scale badgeMult badgeMult (mkStatusBadgeBackground d) <@< { opacity = if high 1.0 0.3 }
    #! img = if mngmnt
               (img <@< { onclick = onClick (ToggleAlarm roomNo d), local = False })
               img
    = [img : acc]
  | otherwise = acc

mkStatusBadgeBackground :: !Detector -> Image a
mkStatusBadgeBackground (FireDetector  _) = badgeImage <@< { fill = toSVGColor "red"  }
mkStatusBadgeBackground (SmokeDetector _) = badgeImage <@< { fill = toSVGColor "grey" }
mkStatusBadgeBackground (FloodDetector _) = badgeImage <@< { fill = toSVGColor "lightblue" }

mkActorBadge :: !MyActor -> Image a
mkActorBadge {actorStatus = {occupied}, userName, carrying}
  #! actorBadge  = mkActorBadgeBackground occupied
  #! userStr     = toString userName
  #! userInitial = text myFontDef (userStr % (0,0)) <@< { fill = toSVGColor "white" }
  #! actorBadge  = overlay [(AtMiddleX, AtMiddleY)] [] [userInitial] (Just actorBadge)
  #! inventory   = map (\i -> mkInventoryBadge True (toString i % (0, 1))) carrying
  = above (repeat AtMiddleX) [] [actorBadge : inventory] Nothing

mkActorBadgeBackground :: !Availability -> Image a
mkActorBadgeBackground occupied = badgeImage <@< { fill = toSVGColor (case occupied of
                                                                        Available    -> "green"
                                                                        NotAvailable -> "black"
                                                                        Busy         -> "orange")}

mkInventoryBadge :: Bool !String -> Image b
mkInventoryBadge portable str
  #! txt = text myFontDef str <@< { fill = toSVGColor "white" }
  = overlay [(AtMiddleX, AtMiddleY)] [] [txt] (Just (mkInventoryBadgeBackground portable))

mkInventoryBadgeBackground :: Bool -> Image b
mkInventoryBadgeBackground portable
  = wideBadgeImage <@< { fill = toSVGColor (if portable "BlueViolet" "purple") }

badgeImage :: Image a
badgeImage = rect (px 11.0) (px 11.0) <@< { stroke = toSVGColor "black" }
                                      <@< { strokewidth = px 1.0 }

wideBadgeImage :: Image a
wideBadgeImage = rect (px 16.0) (px 11.0) <@< { stroke = toSVGColor "black" }
                                          <@< { strokewidth = px 1.0 }
// ------------

isHigh :: !Detector -> Bool
isHigh (FireDetector  b) = b
isHigh (SmokeDetector b) = b
isHigh (FloodDetector b) = b

toggle b = not b


// should be in the library somewhere

mkTable :: [String]  ![a] -> Table | gText{|*|} a
mkTable	headers a = Table headers (map row a) Nothing
where
	row x =  [Text cell \\ cell <- gText{|*|} AsRow (Just x)]
