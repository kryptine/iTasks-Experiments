definition module scripting
 
import iTasks
 
import shipType


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

myScript :: Shared [Script]

mkScript :: Task [Script]

interperScript ::  (RoomNumber,Detector) User [Script] -> Task Bool

