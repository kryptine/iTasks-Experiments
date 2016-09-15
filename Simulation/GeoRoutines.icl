implementation module GeoRoutines

import StdEnv

//Start = getHeadingToPositionDeg (0.0,3.2) (0.1,3.1) 

:: LatLng :== (!Real,!Real)

earth_radius :: Real
earth_radius = 6371000.0

pi = 3.141592653589

degrees2radials :: Real -> Real
degrees2radials deg = deg * pi / 180.0

radials2degrees :: Real -> Real
radials2degrees rad = rad * 180.0 / pi

fromDegrees :: (Real,Real) -> LatLng
fromDegrees (e,t) = (degrees2radials e,degrees2radials t)

toDegrees ::  LatLng ->(Real,Real)
toDegrees (e,t) = (radials2degrees e,radials2degrees t)

// 3d part
distanceDeg :: LatLng LatLng -> Real
distanceDeg p1 p2 = distance (fromDegrees p1) (fromDegrees p2)

distance :: LatLng LatLng -> Real
distance p1 p2 = alpha * earth_radius
where p1xyz = normalizedlatLong2XYZ p1
      p2xyz = normalizedlatLong2XYZ p2
      ip1p2 = inner3 p1xyz p2xyz
      alpha = acos ip1p2

getDirectionToPositionDeg :: LatLng LatLng -> Real
getDirectionToPositionDeg (lat1,lon1) (lat2,lon2) 
= radials2degrees (getDirectionToPosition (degrees2radials lat1,degrees2radials lon1) (degrees2radials lat2,degrees2radials lon2))

getDirectionToPosition :: LatLng LatLng -> Real
getDirectionToPosition (lat1,lon1) (lat2,lon2) 
= normalizeDirection (atan (x / y) + correction)
where
 x = sin(lon2 - lon1) * cos lat2  
 y = cos lat1 * sin lat2- sin lat1 * cos lat2  * cos(lon2 - lon1)
 correction | y < 0.0 && x < 0.0  = ~pi 
            | y < 0.0 && x > 0.0  = pi
            | y < 0.0 && x == 0.0 = ~pi
            | otherwise           = 0.0
            
normalizedlatLong2XYZ :: LatLng -> (!Real,!Real,!Real)
normalizedlatLong2XYZ (lat,lng) = (r * cos lng,r * sin lng, sin lat)
where r = cos lat

inner3 :: (!Real,!Real,!Real) (!Real,!Real,!Real) -> Real
inner3 (a1,a2,a3) (b1,b2,b3) = a1*b1+a2*b2+a3*b3

inner2 :: (!Real,!Real) (!Real,!Real) -> Real
inner2 (a1,a2) (b1,b2) = a1*b1+a2*b2

normalizeLatLong :: LatLng -> LatLng
normalizeLatLong (lat,lon) = (fst nn, normlon (snd nn))
where nn | lat > pi / 2.0       = (pi - lat, lon + pi)
         | lat < ~pi / 2.0      = (~pi - lat,lon + pi)
         | otherwise            = (lat,lon)
      normlon lon | lon > pi    = normlon (lon - 2.0 * pi)
                  | lon < ~pi   = normlon (lon + 2.0 * pi)
                  | otherwise   = lon

normalizeDirection :: !Real -> Real
normalizeDirection direction | direction < ~pi = direction + 2.0 * pi
                             | direction > pi  = direction - 2.0 * pi
                                               = direction
      
normalizeDirectionDeg :: !Real -> Real
normalizeDirectionDeg direction | direction < -180.0 = direction + 360.0
                                | direction > 180.0  = direction - 360.0
                                                     = direction
      
translateDeg :: LatLng !Real !Real -> LatLng
translateDeg pos dir dist = toDegrees (translate (fromDegrees pos) (degrees2radials dir) dist)

translate :: LatLng !Real !Real -> LatLng
translate (lat,lon) direction distance = normalizeLatLong (newlat,newlon)
where newlat = lat + cos direction * distance / earth_radius
      newlon = lon + sin direction * distance / (earth_radius * cos lat)
      
translateAlongCurveDeg :: LatLng !Real !Real !Real -> LatLng
translateAlongCurveDeg pos direction angle radius 
= toDegrees (translateAlongCurve (fromDegrees pos) 
                       (degrees2radials direction) 
                       (degrees2radials angle) 
                       radius )

translateAlongCurve :: LatLng !Real !Real !Real -> LatLng
translateAlongCurve pos direction angle radius = translate pos (direction + angleDirection) distlinear
where angleDirection = angle / 2.0
      distlinear     = 2.0 * radius * sin angleDirection

turningPoint :: LatLng LatLng LatLng !Real -> LatLng
turningPoint p1 p2 p3 radius = distpoint
where heading12 = getDirectionToPosition p1 p2
      heading23 = getDirectionToPosition p2 p3
      deltaH    = heading23 - heading12
      distTurn  = distanceForTurning deltaH radius
      distpoint = translate p1 heading12 (distance p1 p2 - distTurn)


// 2d functions
distanceForTurning :: !Real !Real -> Real
distanceForTurning deltaAngle radius = radius * (1.0 - cos deltaAngle) / sin deltaAngle

acceleration2radius :: !Real !Real -> Real
acceleration2radius accel speed = speed * speed / accel

angularVelocity2radius :: !Real !Real -> Real
angularVelocity2radius w speed= speed / w

radius2angularVelocity :: !Real !Real -> Real
radius2angularVelocity radius speed= speed / radius

acceleration2angularVelocity :: !Real !Real -> Real
acceleration2angularVelocity accel  speed = accel / speed

acceleration2angularVelocityDeg :: !Real !Real -> Real
acceleration2angularVelocityDeg accel  speed = radials2degrees (accel / speed)

angularVelocity2acceleration :: !Real !Real -> Real
angularVelocity2acceleration w speed = speed / w

radius2acceleration :: !Real !Real -> Real
radius2acceleration  radius  speed = speed * speed / radius

dirDist2vector :: !Real !Real -> (!Real,!Real)
dirDist2vector direction  distance = (distance * sin direction, distance *  cos direction)

dif2 :: (!Real,!Real) (!Real,!Real) -> (!Real,!Real)
dif2 (x1,x2) (y1,y2) = (x1-y1,x2-y2)

normsq :: (!Real,!Real) -> Real
normsq p = inner2 p p

// Relative position in plane
relPosition :: LatLng LatLng -> (!Real,!Real)
relPosition p1 p2 = dirDist2vector (getDirectionToPosition p1 p2) (distance p1 p2)

deltaDirectionDeg :: !Real !Real -> Real
deltaDirectionDeg currentDir nextDir = delta
where 
 deltaTemp = nextDir - currentDir
 delta | deltaTemp < -180.0 = 360.0 + deltaTemp
       | deltaTemp >180.0  = -360.0 + deltaTemp
       | otherwise       = deltaTemp

deltaDirection :: !Real !Real -> Real
deltaDirection currentDir nextDir = delta
where 
 deltaTemp = nextDir - currentDir
 delta | deltaTemp < ~pi = 2.0 * pi + deltaTemp
       | deltaTemp > pi  = -2.0 * pi + deltaTemp
       | otherwise       = deltaTemp
    











