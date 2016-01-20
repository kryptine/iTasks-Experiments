implementation module ShipAdventure.Scripting

import iTasks

import ShipAdventure.Types
import ShipAdventure.PathFinding
import ShipAdventure.Util
import qualified Data.IntMap.Strict as DIS

// scripted simulation

derive class iTask Target, Script, Condition

handleFireScript :: Shared [Script]
handleFireScript = sharedStore "handleFireScript" []

handleFloodScript :: Shared [Script]
handleFloodScript = sharedStore "handleFloodScript" []

handleSmokeScript :: Shared [Script]
handleSmokeScript = sharedStore "handleSmokeScript" []

changeFireScript :: Task ()
changeFireScript = changeScript "Handling Fire" handleFireScript 

changeFloodScript :: Task ()
changeFloodScript = changeScript "Handling Flood" handleFloodScript 

changeSmokeScript :: Task ()
changeSmokeScript = changeScript "Handling Smoke" handleSmokeScript 

changeScript :: String (Shared [Script]) -> Task ()
changeScript prompt script
	=	viewSharedInformation ("Current Script: " <+++ prompt) [ViewWith (map (\e -> toSingleLineText e))] script
	>>*	[OnAction (Action "Fine" []) (always (return ()))
		,OnAction (Action "Change" []) (always (	updateSharedInformation ("Change Script: " <+++ prompt) [] script 
												>>| changeScript prompt script
												))
		]


interperScript ::  (RoomNumber,Detector) User [Script] -> Task Bool
interperScript (targetRoom,detector) user script
  =                get myActorMap
  >>= \actorMap -> case findUser user actorMap of
                     Just user -> perform script user
                     _         -> return False
  where
  perform :: [Script] (RoomNumber,MyActor) -> Task Bool
  perform [] (actorLoc, actor) = return True

  perform [MoveTo target:next] (actorLoc,actor)
    =                 get myStatusMap
    >>= \statusMap -> get myInventoryMap
    >>= \invMap    -> get exitLockShare
    >>= \exitLocks -> let newLoc  = whereIs target actorLoc statusMap invMap exitLocks myMap
                      in  autoMove actorLoc newLoc shipShortestPath actor myStatusMap myActorMap myMap
                          >>| perform next (newLoc,actor) 
  perform [Take objType`:next] (actorLoc,actor)	
    =              get myInventoryMap
    >>= \invMap -> case 'DIS'.get actorLoc invMap of
                     Just inv
                       = case [obj \\ obj=:{Object | objType } <- inv | objType` == objType] of
                           [obj : _] =   pickupObject actorLoc obj actor myActorMap myInventoryMap
                                     >>| perform next (actorLoc,actor)
                           _ = perform next (actorLoc, actor)
                     _ = perform next (actorLoc, actor)
  perform [Drop objType`:next] (actorLoc,actor)
      = case [obj \\ obj=:{Object | objType } <- actor.carrying | objType` == objType] of
          [obj : _]
            =   dropObject actorLoc obj actor myActorMap myInventoryMap
            >>| perform next (actorLoc,actor)
          _ = perform next (actorLoc,actor)
  perform [Use objType`:next] (actorLoc,actor)	
      = case [obj \\ obj=:{Object | objType } <- actor.carrying | objType` == objType] of
          [obj : _]
            =   useObject actorLoc obj actor myActorMap
            >>| perform next (actorLoc,actor)
          _ = perform next (actorLoc,actor)
  perform [ReSetTargetDetector:next] (actorLoc,actor)	
    =   setAlarm actor.userName (targetRoom,detector) False myStatusMap
    >>| perform next (actorLoc,actor)
  perform [If condition script1 script2:next] (actorLoc,actor)
    =              get myInventoryMap
    >>= \invMap -> case 'DIS'.get actorLoc invMap of
                     Just inv
                       | isTrue inv condition myMap (actorLoc,actor) = perform (script1 ++ next) (actorLoc, actor)
                       | otherwise                                   = perform (script2 ++ next) (actorLoc, actor)

  isTrue inv (ObjectInCurrentRoom object) map (actorLoc,actor)
    = objTypeInList object inv
  isTrue inv (CarriesObject object) map (actorLoc,actor)
  	= isCarrying object actor
  isTrue inv (ActorStatus status) map (actorLoc,actor)
  	= status === actor.actorStatus
  isTrue inv (And cond1 cond2) map (actorLoc,actor)
  	= and [isTrue inv cond1 map (actorLoc,actor), isTrue inv cond2 map (actorLoc,actor)] 
  isTrue inv (Or cond1 cond2) map (actorLoc,actor)
  	= or [isTrue inv cond1 map (actorLoc,actor), isTrue inv cond2 map (actorLoc,actor)] 


  whereIs (Room nr) actorLoc _ _ _ curMap                                   = nr
  whereIs (Nearest object) actorLoc statusMap inventoryMap exitLocks curMap = let (_,_,_,_,(objectLoc,_,_)) = smartShipPathToClosestObject object actorLoc targetRoom statusMap inventoryMap exitLocks curMap in objectLoc
  whereIs TargetRoom actorLoc _ _ _ curMap                                  = targetRoom

