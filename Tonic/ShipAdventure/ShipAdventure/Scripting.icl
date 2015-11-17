implementation module ShipAdventure.Scripting
 
import iTasks
 
import ShipAdventure.Types
import ShipAdventure.PathFinding

// scripted simulation

:: Target		= 	Room Int
				|	Nearest Object
				|	TargetRoom
:: Script		=	MoveTo Target
				|	Take Object
				|	Drop Object
				|	Use Object
				|	ReSetTargetDetector 
				|	If Condition [Script] [Script]
:: Condition	=	ObjectInCurrentRoom Object
				|	CarriesObject Object
				|	ActorStatus ActorStatus
				|	And Condition Condition
				|	Or Condition Condition

derive class iTask Target, Script, Condition

myScript :: Shared [Script]
myScript = sharedStore "myScript" []

mkScript :: Task [Script]
mkScript = updateSharedInformation "my script" [] myScript

interperScript ::  (RoomNumber,Detector) User [Script] -> Task Bool
interperScript (targetRoom,detector) user script
	= 						get myMap
	>>= \map			->	perform script (fromJust (findUser user map))
where
	perform :: [Script] (RoomNumber,MyActor) -> Task Bool
	perform [] (actorLoc,actor)								
		=	return True	

	perform [MoveTo target:next] (actorLoc,actor)	
		=					get myMap
		>>= \curMap ->		let newLoc  = whereIs target actorLoc curMap 
							in	autoMove actorLoc newLoc shipShortestPath actor myMap
								>>| perform next (newLoc,actor) 
	perform [Take object:next] (actorLoc,actor)	
		=					pickupObject actorLoc object actor myMap
		>>|					perform next (actorLoc,actor)
	perform [Drop object:next] (actorLoc,actor)	
		=					dropDownObject actorLoc object actor myMap
		>>|					perform next (actorLoc,actor)
	perform [Use object:next] (actorLoc,actor)	
		=					useObject actorLoc object actor myMap
		>>|					perform next (actorLoc,actor)
	perform [ReSetTargetDetector:next] (actorLoc,actor)	
		=					setAlarm actor.userName (targetRoom,detector) False myMap
		>>|					perform next (actorLoc,actor)
	perform [If condition script1 script2:next] (actorLoc,actor)
		=					get myMap
		>>= \map ->			if (isTrue condition map (actorLoc,actor))  (perform (script1 ++ next) (actorLoc,actor)) 
												      					(perform (script2 ++ next) (actorLoc,actor))

	isTrue (ObjectInCurrentRoom object) map (actorLoc,actor)
		=	isMember object (fromJust (getRoomFromMap actorLoc map)).inventory
	isTrue (CarriesObject object) map (actorLoc,actor)
		=	isMember object actor.carrying
	isTrue (ActorStatus status) map (actorLoc,actor)
		=	status === actor.actorStatus
	isTrue (And cond1 cond2) map (actorLoc,actor)
		=	and [isTrue cond1 map (actorLoc,actor), isTrue cond2 map (actorLoc,actor)] 
	isTrue (Or cond1 cond2) map (actorLoc,actor)
		=	or [isTrue cond1 map (actorLoc,actor), isTrue cond2 map (actorLoc,actor)] 


	whereIs (Room nr) actorLoc curMap			= nr
	whereIs (Nearest object) actorLoc curMap	= let (_,(objectLoc,_,_)) = smartShipPathToClosestObject object actorLoc targetRoom curMap in objectLoc
	whereIs TargetRoom actorLoc curMap		    = targetRoom

