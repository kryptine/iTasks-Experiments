definition module MovingEntity

import GeoRoutines, iTasks

::MovingEntity = {id          :: !Int
                 ,position    :: LatLng
                 ,altitude    :: !Real
                 ,direction   :: !Real
                 ,speed       :: !Real
                 ,vertSpeed   :: !Real
                 ,angVelocity :: !Real                 
                 ,timeLate    :: !Int
                 ,properties  ::  EntityProperties
                 }

/*
* maxAccel determines the radius of the smallest turning circle
* e.g. 40 m/s^2 (4g) for an fighter plane
*/
:: EntityProperties = {maxSpeed :: !Real
                      ,maxAccel :: !Real
                      }

/* Create a new moving entity with some default values
*  @param id
*  @param start position in latlon coordinates
*  @param speed
*  @param time of creation (in seconds)
*/                 
newMovingEntity    :: Int LatLng !Real !Int -> MovingEntity
newMovingEntityDeg :: Int LatLng !Real !Int -> MovingEntity

/* Update the position of the entity given its current position and speed
*  Note: 2 version, one with positions in radials and one degrees
*  @param moving entity
*  @param list of waypoint the entity has to visit
*  @param postition of waypoint in list it is currently aiming for (initial 0)
*  @param current time in econds since start of simulation
*/
moveAlongWayPoints       :: MovingEntity [LatLng] Int Int -> (MovingEntity,Int)
moveAlongWayPointsDeg    :: MovingEntity [LatLng] Int Int -> (MovingEntity,Int)

updatePosition :: MovingEntity !Int -> MovingEntity
updatePositionDeg :: MovingEntity !Int -> MovingEntity

moveToTarget    :: MovingEntity LatLng !Int -> MovingEntity        
moveToTargetDeg :: MovingEntity LatLng !Int -> MovingEntity        
