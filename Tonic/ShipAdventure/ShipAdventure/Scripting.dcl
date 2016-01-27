definition module ShipAdventure.Scripting
 
import iTasks
 
import ShipAdventure.Types


// script language


:: Target		= 	Room Int
				|	Nearest ObjectType
				|	TargetRoom
:: Script		=	MoveTo Target
				|	Take ObjectType
				|	Drop ObjectType
				|	Use ObjectType
				|	ReSetTargetDetector 
				|	If Condition [Script] [Script]
:: Condition	=	ObjectInCurrentRoom ObjectType
				|	CarriesObject ObjectType
				|	ActorStatus ActorStatus
				|	And Condition Condition
				|	Or Condition Condition

derive class iTask Target, Script, Condition

handleFireScript 	:: Shared [Script]
handleFloodScript 	:: Shared [Script]
handleSmokeScript 	:: Shared [Script]

changeFireScript	:: Task ()
changeFloodScript 	:: Task ()
changeSmokeScript 	:: Task ()

interperScript 		::  (RoomNumber, RoomStatus) User [Script] -> Task Bool

