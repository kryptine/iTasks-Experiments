module GeoExample

import StdEnv, PView, Derived
import Data.Void, Data.Error, Data.Func, Data.Either, Text.JSON
from Data.List import splitWith

//GEOGRAPHIC EXAMPLE

// In local memory share
:: GeoPerspective =
    { center :: (Int,Int)
    , bounds :: Maybe (Int,Int,Int,Int)
    }

// Used in a task
:: GeoMap =
    { perspective :: GeoPerspective
    , markers :: [(String,(Int,Int))]
    }

// Lists in database
:: Contact =
    { name     :: String
    , type     :: String
    , position :: (Int,Int)
    }

inBounds :: (Int,Int,Int,Int) Contact -> Bool
inBounds (minx,miny,maxx,maxy) {position=(x,y)}
    = x >= minx && x <= maxx && y >= miny && y <= maxy

contactMarker :: Contact -> (String,(Int,Int))
contactMarker {Contact|name,type,position} = (name+++" ("+++type+++")",position)

derive JSONEncode GeoPerspective,GeoMap,Contact
derive JSONDecode GeoPerspective,GeoMap,Contact

allShips :: PView Void [Contact] [Contact] MyWorld
allShips = createStoreView "allShips" ships
where
    ships =
        [{name = "SA",type = "Ship", position = (1,1)}
        ,{name = "SB",type = "Ship", position = (2,3)}
        ,{name = "SC",type = "Ship", position = (3,2)}
        ]

shipView :: PView (Either String (Int,Int,Int,Int)) [Contact] [Contact] MyWorld
shipView = union shipsByName shipsByBounds 
				(\_ is ws bounds -> let (ds,_) = splitWith (inBounds bounds) is in any (inBounds bounds) (ds ++ ws))
				(\_ is ws name -> let (ds,_) = splitWith (\p->p.Contact.name == name) is in any (\p->p.Contact.name == name) (ds ++ ws))

shipsByName :: PView String [Contact] [Contact] MyWorld
shipsByName = applySplit allShips (listFilterSplit \name s -> s.Contact.name == name) tr1

shipsByBounds :: PView (Int,Int,Int,Int) [Contact] [Contact] MyWorld
shipsByBounds = applySplit allShips (listFilterSplit \bounds -> inBounds bounds) tr1

allPlanes :: PView Void [Contact] [Contact] MyWorld
allPlanes = createStoreView "allPlanes" planes
where
    planes =
        [{name = "PA",type = "Plane", position = (2,5)}
        ,{name = "PB",type = "Plane", position = (2,2)}
        ,{name = "PC",type = "Plane", position = (6,6)}
        ]

planeView :: PView (Either String (Int,Int,Int,Int)) [Contact] [Contact] MyWorld
planeView = union planesByName planesByBounds 
				(\_ is ws bounds -> let (ds,_) = splitWith (inBounds bounds) is in any (inBounds bounds) (ds ++ ws))
				(\_ is ws name -> let (ds,_) = splitWith (\p->p.Contact.name == name) is in any (\p->p.Contact.name == name) (ds ++ ws))

planesByName :: PView String [Contact] [Contact] MyWorld
planesByName = applySplit allPlanes (listFilterSplit \name s -> s.Contact.name == name) tr1

planesByBounds :: PView (Int,Int,Int,Int) [Contact] [Contact] MyWorld
planesByBounds = applySplit allPlanes (listFilterSplit \bounds -> inBounds bounds) tr1

contactView :: PView (Either String (Int,Int,Int,Int)) [Contact] [Contact] MyWorld
contactView = joinLists shipView planeView splitter
where
    splitter c=:{Contact|type} = (type == "Ship")

makeMapView :: (PView Void GeoPerspective GeoPerspective MyWorld)
            -> (PView Void GeoMap GeoPerspective MyWorld)
makeMapView perspective = pseq perspective contacts paramF writeF readF
where
    paramF {GeoPerspective|bounds} = fmap Right bounds
    writeF perspective = (perspective,Void)
    readF perspective contacts = {GeoMap|perspective=perspective,markers=map contactMarker contacts}

    contacts = applyLens (maybeParam [] contactView) readOnlyLens

Start world
	# myworld = createMyWorld world
	
/* OK
	# (val, myworld) = get (fixP planeView (Left "PB")) myworld
	# (val, myworld) = get (fixP planeView (Right (2,3,3,6))) myworld
*/

/* OK
 	# myworld = registerForNotification planeView (Right (2,3,3,6)) "PC"  myworld	
 	# (_,myworld) = put (fixP planeView (Left "PC")) [{name = "PC",type = "Plane", position = (2,5)}] myworld
	# (val, myworld) = get (fixP planeView (Right (2,3,3,6))) myworld
*/

    # (p1,myworld) = createMemoryView {center=(3,3),bounds=Just (2,3,3,6)} myworld
	# myworld = registerForNotification (makeMapView p1) Void "p1"  myworld	

// OK
//	# (_,myworld) = put p1 {center=(3,3),bounds=Just (2,3,3,6)} myworld
// OK
//	# (_,myworld) = put (makeMapView p1) {center=(3,3),bounds=Just (2,3,3,6)} myworld
// OK
// 	# (_,myworld) = put (fixP planeView (Left "PA")) [{name = "PA",type = "Plane", position = (3,5)}] myworld
// OK
// 	# (_,myworld) = put (fixP planeView (Left "PA")) [{name = "PA",type = "Plane", position = (7,7)}] myworld
// OK
// 	# (_,myworld) = put (fixP planeView (Left "PB")) [{name = "PB",type = "Plane", position = (7,7)}] myworld

	# (val, myworld) = get (makeMapView p1) myworld

	= (val, getWorld myworld)
	
	
	
	
	
	
	
