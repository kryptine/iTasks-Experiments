module shipAdventure
 
import adventure
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin



:: MyRoom		:== Room RoomStatus Object ActorStatus
:: RoomStatus 	=	Normal
				|	Alert [Detector]
:: Detector		=	Fire 
				| 	Smoke 
:: Object 		= 	FireExtinguisher
				| 	Blanket
:: ActorStatus	= 	{ occupied	:: Availability
					, todo		:: [Instruction]
					}
:: Availability	=	Available | NotAvailable | Occupied | HighPriorityTask  
:: Instruction	=	Goto Int ToDo | WaitFor User
:: ToDo			=	FightFire | InspectRoom 
:: MyMap		:== MAP RoomStatus Object ActorStatus
:: MyActor		:== Actor Object ActorStatus

derive class iTask RoomStatus, Detector, Object, ActorStatus, ToDo, Availability, Instruction

instance == Object 		where (==) o1 o2 = o1 === o2
instance == Instruction where (==) o1 o2 = o1 === o2

myMap  :: Shared MyMap
myMap = sharedStore "myBuilding" [floor0]
where
	floor0  	= [[room1,room2,room3],[corridor],[room4,room5,room6]]
	room1		= {name = "room 1",   number = 1, roomStatus = Normal, inventory = [], exits = [South 4], actors = []}			
	room2		= {name = "room 2",   number = 2, roomStatus = Normal, inventory = [], exits = [South 4], actors = []}			
	room3		= {name = "room 3",   number = 3, roomStatus = Normal, inventory = [FireExtinguisher], exits = [South 4], actors = []}
	corridor	= {name = "corridor", number = 4, roomStatus = Normal, inventory = [], exits = [North 1, North 2, North 3
																						   ,South 5, South 6, South 7
																						   ], actors = []}
	room4		= {name = "room 4",   number = 5, roomStatus = Normal, inventory = [], exits = [North 4], actors = []}			
	room5		= {name = "room 5",   number = 6, roomStatus = Normal, inventory = [], exits = [North 4], actors = []}			
	room6		= {name = "room 6",   number = 7, roomStatus = Normal, inventory = [FireExtinguisher], exits = [North 4], actors = []}
			
// tasks on this specific MAP type

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "Adventure" myTasks)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
		, publish "/map"   (WebApp []) (\_-> showMap)
        ] world
 
myTasks :: [Workflow]
myTasks = 	[	workflow "follow instructions"	"walk around and follow given instructions"  (get currentUser >>= \me -> actorWithInstructions me)
			,	workflow "give instructions"	"give instructions to a worker" 			giveInstructions			
		 	]

showMap = 		viewSharedInformation "Map Status" [] myMap 


actorWithInstructions :: User  -> Task ()
actorWithInstructions user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addActorToMap (newActor user) loc handleInstructions myMap

newActor user 			
	= {userName = user, carrying = [], actorStatus = {todo = [], occupied = Available}}
 
handleInstructions map room actor 
	= if (isEmpty actor.actorStatus.todo)
		(				viewInformation "ToDo list is empty" [] () 
			>>| 		return  actor
		)
		(				enterMultipleChoice "ToDo list" [] actor.actorStatus.todo 
			>>= \done ->return (adjustToDoList actor done)
		)
where
	adjustToDoList actor done
	# ntodo = removeMembers done actor.actorStatus.todo
	= {actor & actorStatus = {todo = ntodo, occupied = if (isEmpty ntodo) Available actor.actorStatus.occupied}}

giveInstructions :: Task ()
giveInstructions 
	= forever
	  (		(	selectSomeOne
	  			-&&-
	  			enterInformation "Define Instruction" []
				-&&-
				enterChoice "Assign Status" [] [Available, NotAvailable, Occupied, HighPriorityTask]
			 )	
	  >>* 	[ OnAction  ActionOk (hasValue (\((roomNumber,actor),(instructions,urgency)) 
	 									-> 	updActorStatus actor.userName (\st -> {st & occupied = urgency
	 																				  , todo = 	instructions ++ st.todo
	 																			   }) myMap
	 													)
	 											)
            , OnAction  ActionCancel (always (return ()))
            ]
	 )


selectSomeOne :: Task (Int,MyActor)
selectSomeOne 
	=		viewInformation "Select worker to give instructions to..." [] ()
	>>*		[	OnAction (Action "AnyOne" []) 			(always (selectSomeOneWith noRestriction))
			,	OnAction (Action "Free Available" [])   (always (selectSomeOneWith isAvailable))
			] 
	
noRestriction _   = True
isAvailable actor = actor.actorStatus.occupied ===  Available

selectSomeOneWith :: ((MyActor) -> Bool) -> Task (Int,MyActor) 
selectSomeOneWith pred
	=	whileUnchanged myMap 
			\mymap ->  enterChoice "Assign worker" [] [(i,actor) \\ (i,actor) <- findAllActors mymap | pred actor ]


