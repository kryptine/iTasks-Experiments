implementation module Adventure.Logging
 
import iTasks
 
derive class iTask Log

myLog :: Shared [Log]						// logging events					
myLog = sharedStore "myLog" []

addLog :: a b c -> Task () | toString a & toString b & toString c
addLog who location about
	=				 get currentDateTime
	>>= \dateTime -> upd (\log -> [{ fromWho = (toString who), when = dateTime, intendedFor = toString location, about = toString about}:log]) myLog
	>>|				 return ()

showLog :: Task [Log]
showLog
	=				viewSharedInformation "Loggings..." [] myLog



