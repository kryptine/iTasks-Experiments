module IOTInterface
/**
* Small experiment to have IOT devices execute simple tasks.
* Instead of the usual full-blown web-ui, tasks are exposed through a
* simple protocol over TCP
*/

import iTasks
import qualified Data.Map as DM
import qualified Data.Queue as DQ
import Text
from Data.Queue import :: Queue

//NOT REALLY API YET
from iTasks._Framework.TaskStore import taskEvents 
derive class iTask Event, Queue

PORT :== 9000

//=== Example ===
iotExample
	= 	exposeTasksForDevice PORT //This enables a device to connect and receive
	||- allTasks 
		[viewMessageOnDevice "Hello World" >>- \_ -> viewInformation "Device completed the task" [] ()
		,enterInformationWithDevice >&> viewSharedInformation "Reading from device:" [] @! ()
		]
	||- forever (viewInformation "DEBUG" [] () >>| return ())
	
//=== API ===
viewMessageOnDevice :: String -> Task ()
viewMessageOnDevice message 
	= onDevice "view" message
	 	(viewInformation () [] message >>* [OnAction (Action "DONE" []) (always (return ()))])

enterInformationWithDevice :: Task String
enterInformationWithDevice
	= onDevice "enter" ""
		(enterInformation () [])

onDevice :: String String (Task a) -> Task a | iTask a
onDevice type args task
	=	parallel [(Embedded,viewProgress),(Detached attr False, \_ -> task)] [] @? result
where
	attr = 'DM'.fromList [("device","generic"),("device-task",type),("device-task-args",args)]
	viewProgress _ = viewInformation () [] "Waiting for device..." @? const NoValue

	result (Value [_,(_,v)] _)	= v
	result _					= NoValue

//=== Interface task ===

exposeTasksForDevice :: Int -> Task ()
exposeTasksForDevice port = tcplisten 9000 True sds {ConnectionHandlers|onConnect=onConnect,whileConnected=whileConnected,onDisconnect} @! ()
where
	//Filtered version of the global task instance administration
	sds = (sdsFocus ("device","generic") taskInstancesByAttribute) |+< taskEvents

	onConnect host (instances,events)
		= (Ok (), Nothing, ["WELCOME ","Hello task robot device!","\r\n"],False)

	whileConnected (Just cmd) _ (instances,events)
		= case parseCommand cmd of	
			(Ok ("LIST",Nothing))
				= (Ok (), Just (resetAll instances events), printWorkList instances, False)
			(Ok ("VALUE",Just (instanceNo,value)))
				# event = (instanceNo, EditEvent (TaskId instanceNo 0) "v" (JSONString value))
				= (Ok (), Just ('DQ'.enqueue event events), ["OK ",toString instanceNo,"\r\n"],False)
			(Ok ("DONE", Just (instanceNo,_)))
				# event = (instanceNo, ActionEvent (TaskId instanceNo 0) "DONE")
				= (Ok (), Just ('DQ'.enqueue event events) , ["OK ",toString instanceNo,"\r\n"],False)
			(Ok ("EXIT", Nothing))
				= (Ok (), Nothing , ["BYE","\r\n"],True)
			(Error e)
				= (Ok (), Nothing, ["ERROR ",e,"\r\n"], False)
			_
				= (Ok (), Nothing, ["ERROR ","Unknown command","\r\n"], False)

	whileConnected Nothing _ (instances,events)
		= (Ok (), Nothing,[],False)
	
	onDisconnect () (instances,events)
		= (Ok (), Nothing)

	//Queue a reset event for all task instances to enable them to accept input
	resetAll instances events = foldl (\q e -> 'DQ'.enqueue e q) events [(instanceNo,ResetEvent) \\ {TaskInstance|instanceNo} <- instances]

	//Dump a simple ASCII version of the task administration
	printWorkList instances = map printWorkItem instances
	printWorkItem {TaskInstance|instanceNo,attributes}
		= join " " ["TASK:", toString instanceNo,fromMaybe "-" ('DM'.get "device-task" attributes)
		  				  ,"[" +++ (fromMaybe "-" ('DM'.get "device-task-args" attributes)) +++ "]"
		  				  ] +++ "\r\n"

	parseCommand cmd
		| not (endsWith "\r\n" cmd) = Error "Command not properly terminated"
		= case split " " (subString 0 (textSize cmd - 2) cmd) of
			[cmd] = Ok (cmd,Nothing)
			[cmd,instanceNo:value] =  Ok (cmd,Just (toInt instanceNo,join " " value))
			_ 						= Error	"Command in wrong format"

Start w = startEngine iotExample w
