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

// moving around in the map

addActorToMap :: (Actor o a) Int ((MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)) (Shared (MAP r o a)) 
																										-> Task () | iTask r & iTask o & iTask a & Eq o
addActorToMap actor location task smap
	=			get smap
	>>= \map -> case (findMe actor.userName map) of
					Nothing	 	-> 			if (exitsRoom location map)
									(		updateRoom location (entering actor) smap
									>>|		viewInformation ("You are in room " <+++ location <+++ ", now you can walk around") [] ()
									>>|		moveAround actor task smap 
									)(		viewInformation ("Room with number: " <+++ location <+++ " does not exist") [] () >>| return ()
									)
					Just (loc,me) ->		viewInformation ("You are already in room" <+++ loc) [] () >>| return ()

moveAround :: (Actor o a) ((MAP r o a) (Room r o a) (Actor o a) -> Task (Actor o a)) (Shared (MAP r o a)) -> Task () | iTask r & iTask o & iTask a & Eq o
moveAround  actor task smap
	= forever		
		(whileUnchanged smap
			(\map -> let room 	= findRoom actor map 
						 nactor = hd [nactor \\ nactor <- room.actors | nactor.userName == actor.userName]
					 in
					(	 (		(		viewInformation "my room" [] room
								 >>*	[ OnAction (Action ("Take Exit " <+++ exit) []) (always (move nactor room.number (fromExit exit) smap))
										\\ exit <- room.exits
										]
										++ 
										[ OnAction (Action ("Fetch " <+++ object) [])  	(always (pickupObject nactor room object smap))
										\\ object <- room.inventory
										]
										++
										[ OnAction (Action ("Drop " <+++ object) [])  	(always (dropDownObject nactor room object smap))
										\\ object <- nactor.carrying
										]
								)
								-||-
								(				task map room nactor
								>>= \actor ->	updateRoom room.number (updateActor actor) smap
								>>|				return ()
								)
 
						)
			    )
			)
		  )
where
	pickupObject actor room object smap
		=				updateRoom room.number (fetchObject object) smap
		>>|				return {actor & carrying = [object:actor.carrying]}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>|				return ()
	
	dropDownObject actor room object smap
		=				updateRoom room.number (dropObject object) smap
		>>|				return {actor & carrying = removeMember object actor.carrying}
		>>= \actor ->	updateRoom room.number (updateActor actor) smap
		>>|				return ()

	move actor fromRoom toRoom smap
		= 				updateRoom fromRoom (leaving actor) smap
		>>| 			updateRoom toRoom (entering actor) smap
		>>|				return ()

// room updating

updateRoom :: Int ((Room r o a)-> (Room r o a)) (Shared (MAP r o a))-> Task () | iTask r & iTask o & iTask a
updateRoom roomNumber updRoom smap 
	= 	upd (updateRoom` roomNumber updRoom) smap 
	>>| return ()
where 
	updateRoom` i upd [] 	  			= []
	updateRoom` i upd [floor:floors]   	= [[map updateThisRoom rooms \\ rooms <- floor]: updateRoom` i upd floors]
	where
		updateThisRoom room = if (i == room.number) (upd room)  room

// room updating utility functions

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

// utility functions to find things located in the map

findMe :: User (MAP r o a) ->  Maybe (Int,(Actor o a))
findMe me map 
#	found = [(location,actor) \\ (location,actor) <- findAllActors map | actor.userName == me]
= if (isNil found) Nothing (Just (hd found)) 

findRoom :: (Actor o a) (MAP r o a) -> (Room r o a)
findRoom actor map 
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

findAllRoomNumbers :: (MAP r o a) ->  [Int]
findAllRoomNumbers map = [i \\ (i,actors) <- findAllActors map]

exitsRoom :: Int (MAP r o a) -> Bool
exitsRoom i map = isMember i (findAllRoomNumbers map)

// the definition above need to put in library modulee =================================================================================
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

isNil [] = True
isNil _ = False


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

adjustToDoList actor done
# ntodo = removeMembers done actor.astatus.todo
= {actor & astatus = {todo = ntodo, occupied = if (isNil ntodo) Available actor.astatus.occupied}}

administrate :: User  -> Task ()
administrate user 
	=			enterInformation "Which room do you want to start in?" []
	>>= \loc ->	addActorToMap (newActor user) loc myTask myMap

newActor user 			
	= {userName = user, carrying = [], astatus = {todo = [], occupied = Available}}
 
myTask map room actor 
	= 				enterMultipleChoice "" [] actor.astatus.todo 
	>>= \done -> 	return (adjustToDoList actor done)

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
	 													>>| 	return ()
	 													)
	 											)
	 								 )
            , OnAction  ActionCancel (always (return ()))
            ]
	 )

						





