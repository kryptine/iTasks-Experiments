module adventure
 
import StdArray
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

import StdMisc

:: Room roomStatus object actorStatus
				=	{ name		 :: String
					, number 	 :: Int
					, exits		 :: [Exit]
					, rstatus	 :: roomStatus
					, inventory	 :: [object]
					, actors	 :: [Actor object actorStatus]
					}
:: Exit			=	North Int
				|	East Int
				|	South Int
				|	West Int
				|	Up Int
				|	Down Int
:: Actor o a	=	{ userName	:: User
					, carrying	:: [o]
					, astatus	:: a
					}
:: Floor r o a	:==	[[Room r o a]]
:: MAP r o a	:== [Floor r o a]

derive class iTask Room, Exit, Actor

// small utility functions 

instance == (Actor o a)  where (==) a1 a2 = a1.userName == a2.userName

fromExit :: Exit -> Int
fromExit (North i) = i
fromExit (East i) = i
fromExit (South i) = i
fromExit (West i) = i
fromExit (Up i) = i
fromExit (Down i) = i

// utility functions on Room

leaving :: (Actor o a) (Room r o a) -> (Room r o a) | Eq o
leaving actor room = {room & actors = removeMember actor room.actors}

entering :: (Actor o a) (Room r o a) -> (Room r o a)
entering actor room = {room & actors = [actor:room.actors]}

fetchObject :: o (Room r o a) -> (Room r o a) | Eq o
fetchObject object room = {room & inventory = removeMember object room.inventory}

dropObject ::  o (Room r o a) -> (Room r o a)
dropObject object room = {room & inventory = [object:room.inventory]}

updateActor :: (Actor o a) (Room r o a) -> (Room r o a) | Eq o
updateActor actor room = {room & actors = [actor:removeMember actor room.actors]}

myRoom :: (Actor o a) (MAP r o a) -> (Room r o a)
myRoom actor map 
# rooms	=	[ room 
			\\ floor <- map, layer <- floor, room <- layer, {userName} <- room.actors 
			| actor.userName == userName
			] 
= case rooms of 
	[]  -> abort "cannot find room of actor"
	_	-> hd rooms

findAllActors :: (MAP r o a) ->  [(Int,(Actor o a))]
findAllActors map =	[ (room.number,actor)
					\\ floor <- map, layer <- floor, room <- layer, actor <- room.actors 
					]

// moving around in the map

:: UserActions r o a :== (Room r o a) (Actor o a) -> [TaskCont (Room r o a) (Task (Actor o a))]

moveAround :: ((MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)) (Actor o a) (Shared (MAP r o a)) -> Task (Actor o a) | iTask r & iTask o & iTask a & Eq o
moveAround task actor smap
	= 		
	(	whileUnchanged smap
		(\map -> let room = myRoom actor map in
			(			(				viewInformation "my room" [] room
						>>*				[ OnAction (Action ("Take Exit " <+++ exit) []) (always (move actor room.number (fromExit exit) smap))
										\\ exit <- room.exits
										]
										++ 
										[ OnAction (Action ("Fetch " <+++ object) [])  	(always (pickupObject actor room object smap))
										\\ object <- room.inventory
										]
										++
										[ OnAction (Action ("Drop " <+++ object) [])  	(always (dropDownObject actor room object smap))
										\\ object <- actor.carrying
										]
						)
						-||-
						(				task map room actor 
						>>= \actor ->	updateRoom room.number (updateActor actor) smap
						>>| 			return actor
						)
			)
		)
	  )
	>>= \actor ->	moveAround task actor smap
where
	pickupObject actor room object smap
		=				updateRoom room.number (fetchObject object) smap
		>>|				return {actor & carrying = [object:actor.carrying]}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>| 			return actor
	
	dropDownObject actor room object smap
		=				updateRoom room.number (dropObject object) smap
		>>|				return {actor & carrying = removeMember object actor.carrying}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>| 			return actor

updateRoom :: Int ((Room r o a)-> (Room r o a)) (Shared (MAP r o a))-> Task () | iTask r & iTask o & iTask a
updateRoom roomNumber updRoom smap 
	= 	upd (updateRoom` roomNumber updRoom) smap 
	>>| return ()
where 
	updateRoom` i upd [] 	  			= []
	updateRoom` i upd [floor:floors]   	= [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
	where
		updateThisRoom room = if (i == room.number) (upd room)  room

move :: (Actor o a) Int Int (Shared (MAP r o a))-> Task (Actor o a)| iTask r & iTask o & iTask a & Eq o
move actor fromRoom toRoom smap
	= 				
	  			 	updateRoom fromRoom (leaving actor) smap
	  	>>| 		updateRoom toRoom (entering actor) smap
		>>|			return actor

// defining a concrete map as example

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "Adventure" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
		, publish "/map"   (WebApp []) (\_-> showMap)
        ] world
 
myExamples :: [Workflow]
myExamples = 	[	workflow "walk around"			"you will start in room 1 on the map"  (get currentUser >>= \me -> administrate me)
				,	workflow "give instructions"	"give instructions to a worker" 		giveInstructions			
			 	]

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
	room1		= {name = "room 1",   number = 1, rstatus = Normal, inventory = [], exits = [South 4], actors = []}			
	room2		= {name = "room 2",   number = 2, rstatus = Normal, inventory = [], exits = [South 4], actors = []}			
	room3		= {name = "room 3",   number = 3, rstatus = Normal, inventory = [FireExtinguisher], exits = [South 4], actors = []}
	corridor	= {name = "corridor", number = 4, rstatus = Normal, inventory = [], exits = [North 1, North 2, North 3
																						   ,South 5, South 6, South 7
																						   ], actors = []}
	room4		= {name = "room 4",   number = 5, rstatus = Normal, inventory = [], exits = [North 4], actors = []}			
	room5		= {name = "room 5",   number = 6, rstatus = Normal, inventory = [], exits = [North 4], actors = []}			
	room6		= {name = "room 6",   number = 7, rstatus = Normal, inventory = [FireExtinguisher], exits = [North 4], actors = []}
			
// tasks on this specific MAP type

showMap = 		viewSharedInformation "Map Status" [] myMap 

administrate :: User  -> Task ()
administrate user 
	=			updateRoom 1 (entering actor) myMap
		>>|		viewInformation "you are administrated, now you can walk around" [] ()
		>>|		followInstructions actor
where
	actor = {userName = user, carrying = [], astatus = {occupied = Available,todo = []}}


isNil [] = True
isNil _ = False

followInstructions :: MyActor  -> Task ()
followInstructions actor = moveAround myTask actor myMap >>| return ()

myTask map room actor = 				enterMultipleChoice "" [] actor.astatus.todo 
						>>= \done -> 	let ntodo = removeMembers done actor.astatus.todo in
											return {actor & astatus.todo = ntodo, astatus.occupied = if (isNil ntodo) Available actor.astatus.occupied}

giveInstructions :: Task ()
giveInstructions 
	= forever
	  (		((	enterInformation "Define Instruction" []
				-&&-
				enterChoice "Assign Status" [] [Available, NotAvailable, Occupied, HighPriorityTask]
			)	-&&-
			whileUnchanged myMap 
				(\mymap ->  enterChoice "Assign worker" [] [(i,actor) \\ (i,actor) <- findAllActors mymap | actor.astatus.occupied ===  Available])
			)
	  >>* 	[ OnAction  ActionOk     (hasValue (\((instructions,urgency),(roomNumber,actor)) 
	 									-> 			let nactor = {actor & astatus.occupied = urgency , astatus.todo = instructions} in
	 													(		updateRoom roomNumber (updateActor nactor) myMap
	 													>>|		appendTopLevelTaskFor nactor.userName False (followInstructions nactor) 
	 													>>| 	return ()
	 													)
	 											)
	 								 )
            , OnAction  ActionCancel (always (return ()))
            ]
	 )

						





