definition module GeoRoutines

//import StdEnv

:: LatLng :== (!Real,!Real)

earth_radius :: Real

degrees2radials :: Real -> Real
radials2degrees :: Real -> Real

fromDegrees :: (Real,Real) -> LatLng
toDegrees   ::  LatLng ->(Real,Real)

// 3d part
distance :: LatLng LatLng -> Real
distanceDeg :: LatLng LatLng -> Real

getDirectionToPositionDeg :: LatLng LatLng -> Real
getDirectionToPosition :: LatLng LatLng -> Real

normalizeDirection :: !Real -> Real
normalizeDirectionDeg :: !Real -> Real
                  
translateDeg :: LatLng !Real !Real -> LatLng
translate :: LatLng !Real !Real -> LatLng
      
translateAlongCurve :: LatLng !Real !Real !Real -> LatLng
translateAlongCurveDeg :: LatLng !Real !Real !Real -> LatLng

deltaDirection :: !Real !Real -> Real
deltaDirectionDeg :: !Real !Real -> Real

// 2d part
distanceForTurning :: !Real !Real -> Real
//distanceForTurningDeg :: !Real !Real -> Real

//	acceleration2radius :: !Real !Real -> Real
//	acceleration2radiusDeg :: !Real !Real -> Real
//	
angularVelocity2radius :: !Real !Real -> Real
//	angularVelocity2radiusDeg :: !Real !Real -> Real
//	
//	radius2angularVelocity :: !Real !Real -> Real
//	radius2angularVelocityDeg :: !Real !Real -> Real
//	
acceleration2angularVelocity :: !Real !Real -> Real
acceleration2angularVelocityDeg :: !Real !Real -> Real
//	
//	angularVelocity2acceleration :: !Real !Real -> Real
//	angularVelocity2accelerationDeg :: !Real !Real -> Real
//	
//	radius2acceleration :: !Real !Real -> Real
//	radius2accelerationDeg :: !Real !Real -> Real
//	
//	dirDist2vector :: !Real !Real -> (!Real,!Real)
//	dirDist2vectorDeg :: !Real !Real -> (!Real,!Real)
//	
//	relPosition :: LatLng LatLng -> (!Real,!Real)
//	relPositionDeg :: LatLng LatLng -> (!Real,!Real)











