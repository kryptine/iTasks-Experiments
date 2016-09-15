module Airplane
/**
* This module provides a simple simulation of an airplane flying a route over the Netherlands
*/

import iTasks, StdInt, StdOverloaded, MovingEntity, Data.Tuple
import iTasks.API.Extensions.GIS.GoogleMap

//:: LatLng :== (!Real,!Real)

INITIAL_ROUTE = [(52.187704,4.776612)
				,(52.709971,6.062013)
				,(53.462181,5.567628)
				,(53.324603,4.468995)
				,(51.543222,3.34839)
				,(51.556885,5.095216)
				]

MY_ROUTE = [(52.05586831074774, 5.50140380859375),(52.07444183716456, 6.336364746093759),
            (52.07444183716456, 6.33636474609375),(51.82219818336938, 6.46820068359375), 
            (51.67936786087718, 5.99578857421875),(51.80691653515817, 5.44647216796875)]

ROUTE2 = [(52.9047608002297, 4.7124481201171875),(52.904346653702405, 4.8401641845703125),
         (52.83927653705786, 4.857330322265625),(52.82932091031373, 4.7076416015625)]

SIMULATE_INTERVAL :== 1 //In seconds

derive class iTask MovingEntity, EntityProperties

simulateInteractive :: Task [LatLng]
simulateInteractive = withShared ROUTE2
    \route ->
    (   simulateAirplanePosition route
        >&>
		\mbPos -> interactWithSimulation (route >+| (mapRead fromJust mbPos)) <<@ AfterLayout (tweakControls (\[x:xs] -> [x:map (appFst fill) xs]))
    ) <<@ FullScreen

//Simulate the movement of the airplane
//First very simple simulation. Just round robin version of clock
simulateAirplanePosition :: (Shared [LatLng]) -> Task (LatLng,Int) //Position and heading
simulateAirplanePosition route
	= withShared (newMovingEntity 0 (ROUTE2!! 0) 300.0 0, 1, 0)
		\state ->
			watch (mapRead (\(plane,pos,time) -> ( plane.MovingEntity.position, toInt (plane.MovingEntity.direction))) state)
			-||
			//Step the position
			forever (wait SIMULATE_INTERVAL >>- \_ -> upd newPlanePosition (state >+| route))
where
	newPlanePosition :: ((MovingEntity,Int,Int),[LatLng]) -> (MovingEntity,Int,Int)
	newPlanePosition ((plane,pos,time),route)
		# (plane,pos) = moveAlongWayPointsDeg plane route pos time
		= (plane,pos,time + 1)

//Interact with the running simulation
interactWithSimulation :: (ReadWriteShared ([LatLng],(LatLng,Int)) [LatLng]) -> Task [LatLng]
interactWithSimulation sim
	= updateSharedInformation "Waypoints and position" [UpdateWith toPrj fromPrj] sim
where
	toPrj (route,(pos,heading)) = {GoogleMap|defaultValue & perspective = perspective, markers = markers}
	where
		markers			= waypointMarkers ++ [planeMarker]
		planeMarker		= {GoogleMapMarker|defaultValue & markerId = "plane", title = Just "Airforce one", position = {GoogleMapPosition|lat=fst pos,lng=snd pos}, icon = Just planeIcon }
		planeIcon		= GoogleMapComplexIcon {image="jsf-sprite.png",size=(24,24),origin=(0, 24 * headingIndex),anchor=(12,12)}
		headingIndex	= ((heading + 360) rem 360) / 15
		waypointMarkers = [{GoogleMapMarker|defaultValue & markerId = ("wp-"<+++i), draggable = True, title = Just ("Waypoint " <+++ i), position = {GoogleMapPosition|lat=lat,lng=lng}} \\ (lat,lng) <- route & i <- [1..]]
		perspective		= {GoogleMapPerspective|defaultValue.perspective & center = {lat = 52.948300, lng = 4.776007}, zoom = 10}
		
	fromPrj (route,_) map = route

//Wait for (at least) n seconds
wait :: Int -> Task Void
wait n = get currentTime >>- \start -> watch currentTime >>* [OnValue (\(Value now _) -> if (now >= addSeconds n start) (Just (return Void)) Nothing)]
where
	//ONLY CORRECT FOR n < 60
	addSeconds n t = t + {Time|hour=0,min=0,sec=n}

Start :: *World -> *World
Start world = startEngine simulateInteractive world
