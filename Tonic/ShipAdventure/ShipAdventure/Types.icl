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


derive class iTask Detector, ObjectType, ActorStatus, Availability
derive class iTask Cable, CableConnection, Priority, MapClick

// std overloading instances

//instance == Object      where (==) o1 o2 = o1 === o2
instance == ObjectType  where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2
instance toString ObjectType where
  toString FireExtinguisher = "Extinguiser"
  toString FireBlanket      = "Blanket"
  toString Plug             = "Plug"
  toString Radar            = "Radar"
  toString PowerGen         = "Power generator"
  toString CoolingPump      = "Cooling pump"

instance toString Exit where toString exit = toSingleLineText exit

instance toString Detector
where toString (FireDetector _)  = "Fire Alarm"
	  toString (SmokeDetector _) = "Smoke Alarm"
	  toString (FloodDetector _) = "Flood Alarm"

// shared stores:


myStatusMap :: RWShared () MyRoomStatusMap MyRoomStatusMap
myStatusMap = sharedStore "myStatusMap" 'DIS'.newMap

statusInRoomShare :: RWShared RoomNumber RoomStatus RoomStatus
statusInRoomShare = intMapLens "statusInRoomShare" myStatusMap (Just [])

myInventoryMap :: RWShared () MyRoomInventoryMap MyRoomInventoryMap
myInventoryMap = sharedStore "myInventoryMap" ('DIS'.fromList invs)
  where
  invs = [ (4,  [ {Object | objId = 1,  objType = FireExtinguisher, reusable = False, portable = True, quantity = 1 }
                , {Object | objId = 42, objType = Radar, reusable = True, portable = False, quantity = 1 }
                ])
         , (7,  [ {Object | objId = 2,  objType = FireBlanket, reusable = False, portable = True, quantity = 1 }
                , {Object | objId = 24, objType = PowerGen, reusable = True, portable = False, quantity = 1 }
                ])
         , (8,  [ {Object | objId = 3,  objType = FireExtinguisher, reusable = False, portable = True, quantity = 1 }])
         , (9,  [ {Object | objId = 4,  objType = FireBlanket, reusable = False, portable = True, quantity = 1 }])
         , (10, [ {Object | objId = 5,  objType = FireExtinguisher, reusable = False, portable = True, quantity = 1 }])
         , (14, [ {Object | objId = 6,  objType = FireExtinguisher, reusable = False, portable = True, quantity = 1 }])
         , (17, [ {Object | objId = 7,  objType = FireBlanket, reusable = False, portable = True, quantity = 1 }
                , {Object | objId = 8,  objType = Plug, reusable = False, portable = True, quantity = 1 }])
         , (19, [ {Object | objId = 9,  objType = FireBlanket, reusable = False, portable = True, quantity = 1 }
                , {Object | objId = 10, objType = Plug, reusable = False, portable = True, quantity = 1 }])
         , (20, [ {Object | objId = 11, objType = FireExtinguisher, reusable = False, portable = True, quantity = 1 }])
         ]

myNetwork :: Network
myNetwork =
  { Network
  | cables = 'DIS'.fromList [ (3, [{cableId = 1, description = "Power cable", fromRoom = 5, toRoom = 1, operational = True}])
                            ]
  , devices = 'DIS'.fromList [ (1, 'DIS'.fromList [(1, 42)]) // Radar
                             , (5, 'DIS'.fromList [(1, 24)]) // Power
                             ]
  }

devicesForCable :: MyRoomInventoryMap Cable Network -> [MyObject]
devicesForCable invMap {cableId} {devices}
  # objIds = [objectId \\ (_, cableObjects) <- 'DIS'.toList devices, (cableId`, objectId) <- 'DIS'.toList cableObjects | cableId == cableId`]
  = [ obj \\ obj <- flatten ('DIS'.elems invMap) | isMember obj.objId objIds]

cablesForRoom :: RoomNumber Network -> [Cable]
cablesForRoom roomNo {cables} = case 'DIS'.get roomNo cables of
                                  Just cables -> cables
                                  _           -> []

cutCable :: RoomNumber CableId Network -> Network
cutCable roomNo cableId network = { network & cables = 'DIS'.alter (fmap (findAndCutCable cableId)) roomNo network.cables }
  where
  findAndCutCable cableId cables = [if (cable.cableId == cableId) {cable & operational = False} cable \\ cable <- cables]

patchCable :: RoomNumber CableId Network -> Network
patchCable roomNo cableId network = { network & cables = 'DIS'.alter (fmap (findAndCutCable cableId)) roomNo network.cables }
  where
  findAndCutCable cableId cables = [if (cable.cableId == cableId) {cable & operational = True} cable \\ cable <- cables]

inventoryInRoomShare :: RWShared RoomNumber [MyObject] [MyObject]
inventoryInRoomShare = intMapLens "inventoryInRoomShare" myInventoryMap (Just [])

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
	= updateInformationWithShared "Map Status" [imageUpdate id (mapImage True myMap) (\_ _ -> Nothing) (const snd)] (exitLockShare |+| myInventoryMap |+| myStatusMap |+| myActorMap) NoMapClick
      >>* [OnValue (\tv -> case tv of
                             Value (ToggleAlarm selRoom d)   _ -> Just (updRoomStatus selRoom (updDetector toggleDetector d) myStatusMap >>| setRoomDetectors)
                             Value (ToggleDoor selRoom exit) _ -> Just (toggleExit selRoom exit myMap >>| setRoomDetectors)
                             _ -> Nothing
                   )]

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
showMap = updateInformationWithShared "Map Status" [imageUpdate id (mapImage False myMap) (\_ _ -> Nothing) (const snd)] (exitLockShare |+| myInventoryMap |+| myStatusMap |+| myActorMap) NoMapClick
            >&> withSelection (return ())
                  (\mapClick -> case mapClick of
                                  SelectRoom selRoom -> updateInformationWithShared "Room Status" [imageUpdate (\((((exitLocks, invMap), statusMap), actorMap), a) -> ((exitLocks, invMap, statusMap, actorMap, getRoomFromMap selRoom myMap), a)) (\((exitLocks, invMap, statusMap, actorMap, room), _) -> roomImage exitLocks invMap statusMap actorMap True room) (\_ _ -> Nothing) (const snd)] (exitLockShare |+| myInventoryMap |+| myStatusMap |+| myActorMap) NoMapClick
                                  _ -> return NoMapClick)

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
mapImage :: !Bool !DungeonMap !(!(!(!(!RoomExitLockMap, !MyRoomInventoryMap), !MyRoomStatusMap), !MyRoomActorMap), MapClick) !*TagSource -> Image (a, MapClick)
mapImage mngmnt m ((((exitLocks, inventoryMap), statusMap), actorMap), _) tsrc
  #! (floors, tsrc) = mapSt (floorImage exitLocks inventoryMap statusMap actorMap mngmnt) (zip2 m (reverse [0..length m])) tsrc
  #! allFloors      = beside (repeat AtMiddleY) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) floors) Nothing
  #! legendElems    = [ (mkStatusBadgeBackground (FireDetector True),  "Fire detected")
                      , (mkStatusBadgeBackground (SmokeDetector True), "Smoke detected")
                      , (mkStatusBadgeBackground (FloodDetector True), "Flood detected")
                      , (mkActorBadgeBackground Available,             "Available person")
                      , (mkActorBadgeBackground NotAvailable,          "Unavailable person")
                      , (mkActorBadgeBackground Busy,                  "Busy person")
                      , (mkInventoryBadgeBackground,                   "Room inventory")
                      , (mkUpDown 0 (Up 0) 'DM'.newMap,                "Staircase up")
                      , (mkUpDown 0 (Down 0) 'DM'.newMap,              "Staircase down")
                      ]
  #! legendElems    = map (\(img, descr) -> beside (repeat AtMiddleY) [] [img, text myFontDef (" " +++ descr)] Nothing) legendElems
  #! legend         = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) legendElems) Nothing
  = beside [] [] [allFloors, empty (px 8.0) (px 8.0), legend] Nothing

floorImage :: !RoomExitLockMap !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Bool !(!Floor, !Int) !*TagSource -> *(!Image (a, MapClick), !*TagSource)
floorImage exitLocks inventoryMap statusMap actorMap mngmnt (floor, floorNo) [(floorTag, uFloorTag) : tsrc]
  #! (rooms, tsrc) = mapSt f floor tsrc
  #! floor         = tag uFloorTag (above (repeat AtMiddleX) [] [text myFontDef ("Deck " +++ toString floorNo) : rooms] Nothing)
  = (floor, tsrc)
  where
  f :: ![Room] !*TagSource -> *(!Image (a, MapClick), !*TagSource)
  f rooms tsrc
    #! (rooms`, tsrc) = mapSt (roomImage` inventoryMap statusMap actorMap mngmnt False exitLocks) rooms tsrc
    = (beside (repeat AtMiddleY) [] rooms` Nothing, tsrc)

roomDim =: 64.0
exitWidth =: 16.0

myFontDef = normalFontDef "Arial" 10.0

roomImage :: !RoomExitLockMap !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Bool !(Maybe Room) !*TagSource -> Image (a, MapClick)
roomImage exitLocks inventoryMap statusMap actorMap zoomed (Just room) tsrc = fst (roomImage` inventoryMap statusMap actorMap False zoomed exitLocks room tsrc)
roomImage _ _ _ _ _ _ _                                                     = empty zero zero

roomImage` :: !MyRoomInventoryMap !MyRoomStatusMap !MyRoomActorMap !Bool !Bool !RoomExitLockMap !Room !*TagSource -> *(!Image (a, MapClick), !*TagSource)
roomImage` inventoryMap statusMap actorMap mngmnt zoomed exitLocks room=:{number, exits} tsrc
  #! (northEs, eastEs, southEs, westEs, upEs, downEs) = foldr foldExit ([], [], [], [], [], []) exits
  #! numNorth       = length northEs
  #! numSouth       = length southEs
  #! numEast        = length eastEs
  #! numWest        = length westEs
  #! widthMul       = toReal (max (max numNorth numSouth) 1)
  #! heightMul      = toReal (max (max (length eastEs) (length westEs)) 1)
  #! multiplier     = if zoomed 2.0 1.0
  #! bgWidth        = multiplier * roomDim * widthMul
  #! bgHeight       = multiplier * roomDim * heightMul
  #! bg             = rect (px bgWidth) (px bgHeight) <@< { fill = toSVGColor "white" }
  #! bg             = bg <@< { onclick = onClick (SelectRoom number), local = False }
  #! roomStatus     = case 'DIS'.get number statusMap of
                        Just roomStatus = roomStatus
                        _
                        | mngmnt    = [FireDetector False, SmokeDetector False, FloodDetector False] // TODO This isnt really correct yet. When we get Just, other buttons will disappear. We need to fill the shares to make this all work.
                        | otherwise = []
  #! statusBadges   = above (repeat AtMiddleX) [] (foldr (mkStatusBadge number mngmnt multiplier) [] roomStatus) Nothing
  #! actors         = case 'DIS'.get number actorMap of
                        Just actors -> actors
                        _           -> []
  #! actorBadges    = above (repeat AtMiddleX) [] (map (scale multiplier multiplier o mkActorBadge) actors) Nothing
  #! inventory      = case 'DIS'.get number inventoryMap of
                        Just inv -> inv
                        _        -> []
  #! numInv         = length inventory
  #! inventoryBadge = if (numInv > 0)
                        (beside (repeat AtMiddleY) [] (map (\i -> scale multiplier multiplier (mkInventoryBadge (toString i % (0,0)))) inventory) Nothing)
                        (empty zero zero)
  #! roomNo         = text myFontDef (toString number) <@< { onclick = onClick (SelectRoom number), local = False }
  #! upDownExits    = above (repeat AtMiddleX) [] (map (\e -> scale multiplier multiplier (mkUpDown number e exitLocks) <@< { onclick = onClick (ToggleDoor number e), local = False }) (upEs ++ downEs)) Nothing
  #! (topExitAligns, topExitOffsets, topExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numNorth (AtLeft, AtTop)    northEs
  #! (botExitAligns, botExitOffsets, botExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numSouth (AtLeft, AtBottom) southEs
  #! (rExitAligns,   rExitOffsets,   rExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numEast  (AtRight, AtTop)   eastEs
  #! (lExitAligns,   lExitOffsets,   lExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numWest  (AtLeft, AtTop)    westEs
  #! total          = overlay ([(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtMiddleY), (AtLeft, AtBottom), (AtRight, AtBottom)] ++ topExitAligns ++ botExitAligns ++ rExitAligns ++ lExitAligns)
                              ([(px 3.0, px 3.0), (px -3.0, px 3.0), (zero, zero), (px 3.0, px -3.0), (px -3.0, px -3.0)] ++ topExitOffsets ++ botExitOffsets ++ rExitOffsets ++ lExitOffsets)
                              ([statusBadges, actorBadges, roomNo, inventoryBadge, upDownExits] ++ topExitImgs ++ botExitImgs ++ rExitImgs ++ lExitImgs) (Just bg)
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
  #! inventory   = map (\i -> mkInventoryBadge (toString i % (0,0))) carrying
  = above (repeat AtMiddleX) [] [actorBadge : inventory] Nothing

mkActorBadgeBackground :: !Availability -> Image a
mkActorBadgeBackground occupied = badgeImage <@< { fill = toSVGColor (case occupied of
                                                                        Available    -> "green"
                                                                        NotAvailable -> "black"
                                                                        Busy         -> "orange")}

mkInventoryBadge :: !String -> Image b
mkInventoryBadge str
  #! txt   = text myFontDef str <@< { fill = toSVGColor "white" }
  = overlay [(AtMiddleX, AtMiddleY)] [] [txt] (Just mkInventoryBadgeBackground)

mkInventoryBadgeBackground :: Image b
mkInventoryBadgeBackground
  = badgeImage <@< { fill = toSVGColor "purple" }

badgeImage :: Image a
badgeImage = rect (px 11.0) (px 11.0) <@< { stroke = toSVGColor "black" }
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
