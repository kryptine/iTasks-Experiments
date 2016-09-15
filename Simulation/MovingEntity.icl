implementation module MovingEntity

import iTasks, GeoRoutines

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

:: EntityProperties = {maxSpeed :: !Real
                      ,maxAccel :: !Real
                      }
                      
newMovingEntityDeg :: Int LatLng !Real !Int -> MovingEntity
newMovingEntityDeg id position speed timeLate = newMovingEntity id position speed timeLate

newMovingEntity :: Int LatLng !Real !Int -> MovingEntity
newMovingEntity id position speed timeLate = {id = id
                                       ,position    = position
                                       ,altitude    = 0.0
                                       ,direction   = 0.0
                                       ,speed       = speed
                                       ,vertSpeed   = 0.0
                                       ,angVelocity = 0.0
                                       ,timeLate    = timeLate
                                       ,properties  = {maxSpeed = 300.0,maxAccel = 40.0}
                                       }
                                       
updatePositionDeg :: MovingEntity !Int -> MovingEntity
updatePositionDeg me=:{position,direction,speed,angVelocity,altitude,vertSpeed,timeLate} time 
= {me & timeLate = time, position = newpos, direction = newdir, altitude = newalt}
where deltaTime                   = toReal (time - timeLate)
      angVelocityrad              = degrees2radials angVelocity 
      deltaAngleDeg               = angVelocity * deltaTime
      deltaAngle                  = angVelocityrad * deltaTime
      deltaDist                   = speed * deltaTime
      absAngVel                   = abs angVelocityrad
      radius                      = angularVelocity2radius angVelocityrad speed
      posdir | absAngVel < 0.001  = (translateDeg position direction deltaDist,direction)
             | otherwise          = (translateAlongCurveDeg position direction deltaAngleDeg radius,normalizeDirectionDeg (direction + deltaAngleDeg))
      newalt                      = altitude + vertSpeed * deltaTime
      (newpos,newdir)             = posdir

updatePosition :: MovingEntity !Int -> MovingEntity
updatePosition me=:{position,direction,speed,angVelocity,altitude,vertSpeed,timeLate} time 
= {me & timeLate = time, position = newpos, direction = newdir, altitude = newalt}
where deltaTime                   = toReal (time - timeLate)
      deltaAngle                  = angVelocity * deltaTime
      deltaDist                   = speed * deltaTime
      absAngVel                   = abs angVelocity
      radius                      = angularVelocity2radius angVelocity speed
      posdir | absAngVel < 0.0001 = (translate position direction deltaDist,direction)
             | otherwise          = (translateAlongCurve position direction deltaAngle radius,normalizeDirectionDeg (direction + deltaAngle))
      newalt                      = altitude + vertSpeed * deltaTime
      (newpos,newdir)             = posdir
                                       
//distanceToTarget :: MovingEntity LatLng -> Real

moveAlongWayPointsDeg :: MovingEntity [LatLng] Int Int -> (MovingEntity,Int)
moveAlongWayPointsDeg me=:{direction,position,angVelocity} ps pos  time 
 | pos >= length ps = ({me & speed = 0.0},pos)
 | dist2wp < deltaDist = moveAlongWayPointsDeg me ps (pos+1) time
 | otherwise           = (moveToTargetDeg me p time,pos)
where p            = ps !! pos
      dist2wp      = distanceDeg position p
      deltaDist    = toReal (time - me.timeLate) * me.speed 

moveAlongWayPoints :: MovingEntity [LatLng] Int Int -> (MovingEntity,Int)
moveAlongWayPoints me=:{direction,position,angVelocity} ps pos  time 
 | pos >= length ps = ({me & speed = 0.0},pos)
 | dist2wp < deltaDist = moveAlongWayPoints me ps (pos+1) time
 | otherwise           = (moveToTarget me p time,pos)
where p            = ps !! pos
      dist2wp      = distance position p
      deltaDist    = toReal (time - me.timeLate) * me.speed 

// Make entity move in the direction of target pos
moveToTargetDeg :: MovingEntity LatLng !Int -> MovingEntity        
moveToTargetDeg me=:{direction,position,speed,properties} pos time 
= updatePositionDeg {me & direction = newdir,angVelocity = newAngVel} time
where targetdir  = getDirectionToPositionDeg position pos
      deltadir   = deltaDirectionDeg direction targetdir
      deltatime  = toReal (time - me.timeLate)
      angVel     = acceleration2angularVelocityDeg properties.maxAccel speed
      newdir | abs deltadir < deltatime * angVel = targetdir
             | otherwise                         = direction 
      newAngVel | abs deltadir < deltatime * angVel = 0.0
                | deltadir < 0.0                    = ~angVel
                | otherwise                         = angVel

// Make entity move in the direction of target pos
moveToTarget :: MovingEntity LatLng !Int -> MovingEntity        
moveToTarget me=:{direction,position,speed,properties} pos time 
= updatePosition {me & direction = newdir,angVelocity = newAngVel} time
where targetdir  = getDirectionToPosition position pos
      deltadir   = deltaDirection direction targetdir
      deltatime  = toReal (time - me.timeLate)
      angVel     = acceleration2angularVelocity properties.maxAccel speed
      newdir | abs deltadir < deltatime * angVel = targetdir
             | otherwise                         = direction 
      newAngVel | abs deltadir < deltatime * angVel = 0.0
                | deltadir < 0.0                    = ~angVel
                | otherwise                         = angVel
