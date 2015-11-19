definition module ShipAdventure.Scripting
 
import iTasks
 
import ShipAdventure.Types


// script language

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

handleFireScript 	:: Shared [Script]
handleFloodScript 	:: Shared [Script]
handleSmokeScript 	:: Shared [Script]

changeFireScript	:: Task ()
changeFloodScript 	:: Task ()
changeSmokeScript 	:: Task ()

interperScript 		::  (RoomNumber,Detector) User [Script] -> Task Bool

