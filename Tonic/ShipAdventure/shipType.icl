implementation module shipType
 
//import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data


import adventure

derive class iTask Detector, Object, ActorStatus, Availability, Priority, MapClick

// std overloading instances

instance == Object      where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2
instance toString Object where
  toString FireExtinguisher = "Fire extinguiser"
  toString Blanket          = "Blanket"
  toString Plug             = "Plug"

instance toString Exit where toString exit = toSingleLineText exit

instance toString Detector
where toString (FireDetector _)  = "Fire Alarm"
	  toString (SmokeDetector _) = "Smoke Alarm"
	  toString (FloodDetector _) = "Flood Alarm"

// shared stores:

myMap :: RWShared () MyMap MyMap // map of the ship
myMap = sharedStore "myBuilding" myShip

allAvailableActors :: ReadOnlyShared [(RoomNumber, MyActor)]
allAvailableActors
  = toReadOnly (sdsProject (SDSLensRead readActors) SDSNoWrite myMap)
where
    readActors curMap = Ok [(room,actor) \\ (room,actor) <- findAllActors curMap | actor.actorStatus.occupied === Available ]

allActiveAlarms :: ReadOnlyShared [(RoomNumber, Detector)]
allActiveAlarms
  = toReadOnly (sdsProject (SDSLensRead readAlarms) SDSNoWrite myMap)
where
    readAlarms curMap = Ok [ (number,detector) \\ (number,detectors) <- allRoomStatus curMap
                           , detector <- detectors
                           | isHigh detector]

// detectors setting

// setting and resetting of the detection systems

setRoomDetectors :: Task ()
setRoomDetectors 
	= updateInformationWithShared "Map Status" [imageUpdate id (mapImage True) (\_ _ -> Nothing) (const snd)] myMap NoMapClick
      >>* [OnValue (\tv -> case tv of
                             Value (ToggleAlarm selRoom d)   _ -> Just (updRoomStatus selRoom (updDetector toggleDetector d) myMap >>| setRoomDetectors)
                             Value (ToggleDoor selRoom exit) _ -> Just (toggleExit selRoom exit myMap >>| setRoomDetectors)
                             _ -> Nothing
                   )]

from logging import addLog

setAlarm :: User (RoomNumber,Detector) Bool (Shared MyMap) -> Task ()
setAlarm user (alarmLoc,detector) bool smap
	= 		updRoomStatus alarmLoc (updDetector (if bool setDetector resetDetector) detector) smap
	>>|		addLog user alarmLoc  ("Resets " <+++ detector <+++ " to False.") 

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

// find stuf, shortest path to objects

findClosestObject :: RoomNumber (RoomNumber,Detector) MyMap -> (Maybe RoomNumber,Maybe Object)
findClosestObject  myLoc (alarmLoc,detector) curMap
	= 	case detector of
			(SmokeDetector _) = (Just myLoc,Nothing)
			(FloodDetector _) = case findClosest myLoc Plug curMap of
								 Nothing	-> (Nothing,Nothing)
								 objLoc	 	-> (objLoc,Just Plug)
			(FireDetector _)  = case (findClosest myLoc Blanket curMap, findClosest myLoc FireExtinguisher curMap) of
								 (Nothing,Nothing)  -> (Nothing,Nothing)
								 (Nothing,objLoc)   -> (objLoc,Just FireExtinguisher)
								 (objLoc,Nothing)   -> (objLoc,Just Blanket)
								 (objLoc1,objLoc2)  ->  if (fromJust objLoc1 < fromJust objLoc2)
								 							(objLoc1,Just Blanket) (objLoc2,Just FireExtinguisher)

findClosest roomNumber object curMap
	= 	let revPath = reverse (thd3 (snd (pathToClosestObject object roomNumber curMap)))
		in if (isEmpty revPath) Nothing (Just (fromExit (hd revPath)))
		
pathToClosestObject :: Object RoomNumber MyMap -> (Int,(RoomNumber,Int,[Exit]))  // returns: number of objects found, location of object, distance to object, shortest path to obejct
pathToClosestObject kind actorLoc curMap
	= (numberResources, if (numberResources == 0) (-1,-1,[]) (hd spath))
	where
		numberResources = length spath 
		spath = sortBy (\(i,l1,p1) (j,l2,p2) -> i < j)   [let path = shipShortestPath actorLoc objectLoc curMap in (objectLoc, length path, path) 
													\\ (objectLoc,found) <- findAllObjects curMap | found == kind ]


// general map viewing

showMap :: Task MapClick
showMap = updateInformationWithShared "Map Status" [imageUpdate id (mapImage False) (\_ _ -> Nothing) (const snd)] myMap NoMapClick
            >&> withSelection (return ())
                  (\mapClick -> case mapClick of
                                  SelectRoom selRoom -> updateInformationWithShared "Room Status" [imageUpdate (\(m, a) -> (getRoomFromMap selRoom m, a)) (\(room, _) -> roomImage True room) (\_ _ -> Nothing) (const snd)] myMap NoMapClick
                                  _ -> return NoMapClick)



// definition of the ship layout

myShip = [floor0, floor1]
where
	floor0  	= [ [back0]
                  , [room01, room02, room03]
                  , [corridor0]
                  , [room04, room05, room06]
                  , [room07, room08]
                  ]
	back0		= {name = "back 0",     number = 1,  roomStatus = detectors, inventory = [], exits = [(South 2, False), (South 3, False), (South 4, False)], actors = []}
	room01		= {name = "room 0.1",   number = 2,  roomStatus = detectors, inventory = [], exits = [(North 1, False), (South 5, False), (Down 13, False)], actors = []}			
	room02		= {name = "room 0.2",   number = 3,  roomStatus = detectors, inventory = [], exits = [(North 1, False), (South 5, False)], actors = []}			
	room03		= {name = "room 0.3",   number = 4,  roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 1, False), (South 5, False)], actors = []}
	corridor0	= {name = "corridor 0", number = 5,  roomStatus = detectors, inventory = [], exits = [(North 2, False), (North 3, False), (North 4, False)
                                                                                                     , (South 6, False), (South 7, False), (South 8, False)
                                                                                                     ], actors = []}
	room04		= {name = "room 0.4",   number = 6,  roomStatus = detectors, inventory = [], exits = [(North 5, False), (South 9, False)], actors = []}			
	room05		= {name = "room 0.5",   number = 7,  roomStatus = detectors, inventory = [Blanket], exits = [(North 5, False), (South 9, False)], actors = []}			
	room06		= {name = "room 0.6",   number = 8,  roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 5, False), (South 10, False)], actors = []}
	room07		= {name = "room 0.7",   number = 9,  roomStatus = detectors, inventory = [Blanket], exits = [(North 6, False), (North 7, False)], actors = []}			
	room08		= {name = "room 0.8",   number = 10, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 8, False), (Down 21, False)], actors = []}

	floor1  	= [ [back1]
                  , [room11, room12, room13]
                  , [corridor1]
                  , [room14, room15, room16]
                  , [room17, room18]
                  ]
	back1		= {name = "back 1",     number = 12, roomStatus = detectors, inventory = [], exits = [(South 13, False), (South 14, False), (South 15, False)], actors = []}
	room11		= {name = "room 1.1",   number = 13, roomStatus = detectors, inventory = [], exits = [(North 12, False), (South 16, False), (Up 2, False)], actors = []}
	room12		= {name = "room 1.2",   number = 14, roomStatus = detectors, inventory = [], exits = [(North 12, False), (South 16, False), (East 15, False)], actors = []}
	room13		= {name = "room 1.3",   number = 15, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 12, False), (South 16, False), (West 14, False)], actors = []}
	corridor1	= {name = "corridor 1", number = 16, roomStatus = detectors, inventory = [], exits = [(North 13, False), (North 14, False), (North 15, False)
                                                                                                     , (South 17, False), (South 18, False), (South 19, False)
                                                                                                     ], actors = []}
	room14		= {name = "room 1.4",   number = 17, roomStatus = detectors, inventory = [], exits = [(North 16, False), (South 20, False)], actors = []}
	room15		= {name = "room 1.5",   number = 18, roomStatus = detectors, inventory = [Blanket,Plug], exits = [(North 16, False), (South 20, False)], actors = []}
	room16		= {name = "room 1.6",   number = 19, roomStatus = detectors, inventory = [], exits = [(North 16, False), (South 21, False)], actors = []}
	room17		= {name = "room 1.7",   number = 20, roomStatus = detectors, inventory = [Blanket,Plug], exits = [(North 17, False), (North 18, False)], actors = []}
	room18		= {name = "room 1.8",   number = 21, roomStatus = detectors, inventory = [FireExtinguisher], exits = [(North 19, False), (Up 10, False)], actors = []}
	
	detectors = [FireDetector False,SmokeDetector False,FloodDetector False]


// shortest path given the alarms set on the ship

shipShortestPath :: RoomNumber RoomNumber MyMap -> [Exit]
shipShortestPath startRoomNumber endRoomNumber allRooms = shortestPath cost startRoomNumber endRoomNumber allRooms
  where
  cost detectors = 1 + sum (map detectorCost detectors)
  detectorCost (FireDetector  True) = 1000
  detectorCost (SmokeDetector True) = 250
  detectorCost (FloodDetector True) = 1000
  detectorCost _                    = 0

// making an image from the map ...

mapImage :: !Bool !(!MyMap, MapClick) !*TagSource -> Image (a, MapClick)
mapImage mngmnt (m, _) tsrc
  #! (floors, tsrc) = mapSt (floorImage mngmnt) (zip2 m (reverse [0..length m])) tsrc
  #! allFloors      = beside (repeat AtMiddleY) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) floors) Nothing
  #! legendElems    = [ (mkStatusBadgeBackground (FireDetector True), "Fire detected")
                      , (mkStatusBadgeBackground (SmokeDetector True), "Smoke detected")
                      , (mkStatusBadgeBackground (FloodDetector True), "Flood detected")
                      , (mkActorBadgeBackground Available, "Available person")
                      , (mkActorBadgeBackground NotAvailable, "Unavailable person")
                      , (mkActorBadgeBackground Busy, "Busy person")
                      , (mkInventoryBadgeBackground, "Room inventory")
                      , (mkUpDown (Up 0, False), "Staircase up")
                      , (mkUpDown (Down 0, False), "Staircase down")
                      ]
  #! legendElems    = map (\(img, descr) -> beside (repeat AtMiddleY) [] [img, text myFontDef (" " +++ descr)] Nothing) legendElems
  #! legend         = above (repeat AtLeft) [] ('DL'.intersperse (empty (px 8.0) (px 8.0)) legendElems) Nothing
  = beside [] [] [allFloors, empty (px 8.0) (px 8.0), legend] Nothing

floorImage :: !Bool !(!MyFloor, !Int) !*TagSource -> *(!Image (a, MapClick), !*TagSource)
floorImage mngmnt (floor, floorNo) [(floorTag, uFloorTag) : tsrc]
  #! (rooms, tsrc) = mapSt f floor tsrc
  #! floor         = tag uFloorTag (above (repeat AtMiddleX) [] [text myFontDef ("Deck " +++ toString floorNo) : rooms] Nothing)
  = (floor, tsrc)
  where
  f :: ![MyRoom] !*TagSource -> *(!Image (a, MapClick), !*TagSource)
  f rooms tsrc
    #! (rooms`, tsrc) = mapSt (roomImage` mngmnt False) rooms tsrc
    = (beside (repeat AtMiddleY) [] rooms` Nothing, tsrc)

roomDim =: 64.0
exitWidth =: 16.0

myFontDef = normalFontDef "Arial" 10.0

roomImage :: !Bool !(Maybe MyRoom) !*TagSource -> Image (a, MapClick)
roomImage zoomed (Just room) tsrc = fst (roomImage` False zoomed room tsrc)
roomImage _ _ _                   = empty zero zero

roomImage` :: !Bool !Bool !MyRoom !*TagSource -> *(!Image (a, MapClick), !*TagSource)
roomImage` mngmnt zoomed room=:{number, exits, roomStatus, actors, inventory} tsrc
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
  #! statusBadges   = above (repeat AtMiddleX) [] (foldr (mkStatusBadge number mngmnt multiplier) [] roomStatus) Nothing
  #! actorBadges    = above (repeat AtMiddleX) [] (map (scale multiplier multiplier o mkActorBadge) actors) Nothing
  #! numInv         = length inventory
  #! inventoryBadge = if (numInv > 0)
                        (beside (repeat AtMiddleY) [] (map (\i -> scale multiplier multiplier (mkInventoryBadge (toString i % (0,0)))) inventory) Nothing)
                        (empty zero zero)
  #! roomNo         = text myFontDef (toString number) <@< { onclick = onClick (SelectRoom number), local = False }
  #! upDownExits    = above (repeat AtMiddleX) [] (map (\x=:(e, _) -> scale multiplier multiplier (mkUpDown x) <@< { onclick = onClick (ToggleDoor number e), local = False }) (upEs ++ downEs)) Nothing
  #! (topExitAligns, topExitOffsets, topExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numNorth (AtLeft, AtTop)    northEs
  #! (botExitAligns, botExitOffsets, botExitImgs) = mkAsOsIs multiplier (\sp -> (sp, zero)) (rect (px (exitWidth * multiplier)) (px (4.0 * multiplier))) bgWidth  numSouth (AtLeft, AtBottom) southEs
  #! (rExitAligns,   rExitOffsets,   rExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numEast  (AtRight, AtTop)   eastEs
  #! (lExitAligns,   lExitOffsets,   lExitImgs)   = mkAsOsIs multiplier (\sp -> (zero, sp)) (rect (px (4.0 * multiplier)) (px (exitWidth * multiplier))) bgHeight numWest  (AtLeft, AtTop)    westEs
  #! total          = overlay ([(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtMiddleY), (AtLeft, AtBottom), (AtRight, AtBottom)] ++ topExitAligns ++ botExitAligns ++ rExitAligns ++ lExitAligns)
                              ([(px 3.0, px 3.0), (px -3.0, px 3.0), (zero, zero), (px 3.0, px -3.0), (px -3.0, px -3.0)] ++ topExitOffsets ++ botExitOffsets ++ rExitOffsets ++ lExitOffsets)
                              ([statusBadges, actorBadges, roomNo, inventoryBadge, upDownExits] ++ topExitImgs ++ botExitImgs ++ rExitImgs ++ lExitImgs) (Just bg)
  = (total, tsrc)
  where
  foldExit :: !(!Exit, !Locked) !(![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)]) -> (![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)], ![(!Exit, !Locked)])
  foldExit e=:(North _, _) (northEs, eastEs, southEs, westEs, upEs, downEs) = ([e : northEs], eastEs, southEs, westEs, upEs, downEs)
  foldExit e=:(East _, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, [e : eastEs], southEs, westEs, upEs, downEs)
  foldExit e=:(South _, _) (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, [e : southEs], westEs, upEs, downEs)
  foldExit e=:(West _, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, [e : westEs], upEs, downEs)
  foldExit e=:(Up _, _)    (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, [e : upEs], downEs)
  foldExit e=:(Down _, _)  (northEs, eastEs, southEs, westEs, upEs, downEs) = (northEs, eastEs, southEs, westEs, upEs, [e : downEs])

  mkAsOsIs :: !Real !(Span -> (!Span, !Span)) !(Image (a, MapClick)) !Real !Int !(!XAlign, !YAlign) ![(!Exit, !Locked)]
            -> (![(!XAlign, !YAlign)], ![(!Span, !Span)], [Image (a, MapClick)])
  mkAsOsIs multiplier mkTuple doorImg bgSize num align es
    #! exitAligns  = repeatn num align
    #! exitOffsets = reverse (fst (foldr (\_ (xs, n) -> ([mkTuple (px n) : xs], n + roomDim * multiplier)) ([], (roomDim * multiplier - exitWidth) / 2.0) es))
    #! exitImgs    = map mkDoor es
    = (exitAligns, exitOffsets, exitImgs)
    where
    //mkDoor :: !(!Exit, !Locked) -> Image (a, MapClick)
    mkDoor (exit, locked)
      = doorImg
          <@< { stroke = toSVGColor "black" }
          <@< { strokewidth = px 1.0 }
          <@< { fill = toSVGColor (if locked "black" "white") }
          <@< { onclick = onClick (ToggleDoor number exit), local = False }

//onClick :: !MapClick Int (a, MapClick) -> (a, MapClick)
onClick clck _ (m, _) = (m, clck)

mkUpDown :: !(!Exit, !Locked) -> Image (a, MapClick)
mkUpDown (e=:(Up _), l)
  = polygon Nothing [(px 0.0, px 0.0), (px 12.0, px -12.0), (px 12.0, px 0.0)]
      <@< { opacity = if l 0.3 1.0 }
mkUpDown (e=:(Down _), l)
  = polygon Nothing [(px 0.0, px -12.0), (px 12.0, px 0.0), (px 0.0, px 0.0)]
      <@< { opacity = if l 0.3 1.0 }

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
