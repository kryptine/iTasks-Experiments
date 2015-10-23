definition module logging
 
import iTasks
 
:: Log			=	{ fromWho 		:: String
					, intendedFor	:: String
					, when			:: DateTime
					, about			:: String
 					}

derive class iTask Log

// shared store for logging events

myLog 	:: Shared [Log]						

// tasks for logging:

showLog :: Task [Log]

addLog 	:: a b c -> Task () | toString a & toString b & toString c



