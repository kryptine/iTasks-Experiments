module resourcePersonsExample

import resource

import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "CEFP 2015 iTasks & Tonic examples" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
        ] world

// here follow my resources

:: Crew		=	{	userTitle	:: UserTitle	// user whom I can assign a task to
				,	role		:: OneRole
				,	location	:: Location
				}
:: Location = 	OnShip | NotOnShip
:: OneRole	=	Sailor | Captain | ShipsCook 

derive class iTask Crew, Location, OneRole

// define the crew members and the role they can have

sailorAlice		= {userTitle = "alice",	role = Sailor, 		location = OnShip}
cookAlice		= {userTitle = "alice",	role = ShipsCook, 	location = OnShip}
captainBob 		= {userTitle = "bob",	role = Captain, 	location = OnShip}
sailorCarol		= {userTitle = "carol",	role = Sailor, 		location = OnShip}
sailorDave		= {userTitle = "dave",	role = Sailor, 		location = NotOnShip}
sailorEdward	= {userTitle = "edward",role = Sailor, 		location = OnShip}

myCrew :: (Shared [Resource Crew]) 					// initialize crew database, 100 means available 100% of the time
myCrew = sharedStore "myResources" initResources
where
	initResources = [{name = captainBob,	kind = Reusable, 	available = 100	, inUse = []} // In percentage 
					,{name = sailorAlice, 	kind = Reusable, 	available = 60	, inUse = []}
					,{name = cookAlice, 	kind = Reusable, 	available = 40	, inUse = []}
					,{name = sailorCarol, 	kind = Reusable, 	available = 100	, inUse = []}
					,{name = sailorDave, 	kind = Reusable, 	available = 100	, inUse = []}
					,{name = sailorEdward, 	kind = Reusable, 	available = 0	, inUse = []} // He is sick
					]	

wantSailor :: Int (Resource Crew) -> Amount
wantSailor needed {Resource|name = {role = Sailor, location = OnShip}  } =  needed  // % of the sailors time
wantSailor _ _ = 0

wantAvailableSailor :: Int (Resource Crew) -> Amount
wantAvailableSailor needed {Resource|name = {role = Sailor, location = OnShip} , available } =  if (available >= needed) needed 0  // % of the sailors time
wantAvailableSailor _ _ = 0

wantCook :: Int (Resource Crew) -> Amount
wantCook needed {Resource|name = {role = ShipsCook, location = OnShip} , available } = needed
wantCook _ _ = 0

wantAlice :: (Resource Crew) -> Amount
wantAlice {Resource|name = {userTitle = "alice",role = Sailor, location = OnShip} , available, inUse } 	   = foldl (+) available (map snd inUse)
wantAlice {Resource|name = {userTitle = "alice",role = ShipsCook, location = OnShip} , available, inUse }  = foldl (+) available (map snd inUse)
wantAlice _ = 0

hds [r:rs] = [r]
hds _ = []

// tasks


myExamples :: [Workflow]
myExamples = 	[ workflow "resource" 				"resource"  						test
				, workflow "Manage users" 			"Manage system users..." 			manageUsers
			 	]
		
test = forever (((wantSailor 20,id),myCrew) 			?: ("need all sailors",showIt "sailor request 1" "sail 1")) -&&-
	   forever (((wantAvailableSailor 60,hds),myCrew) 	?: ("need one sailor",showIt "sailor request 2" "sail 2")) -&&-
	   forever (((wantAvailableSailor 60,id),myCrew)    ?: ("need all available sailors",showIt "sailor request 3" "sail 3")) -&&-
	   forever (((wantCook 40,id),myCrew)    			?: ("need a cook",showIt "cook request" "cook")) -&&-
	   forever (((wantAlice,id),myCrew)    				?: ("need alice",showIt "alice request" "alice")) -&&-
	   showResources myCrew -&&-
	   alterResources myCrew

showIt ask result = viewInformation ask [] "" >>* [OnAction ActionOk (always (return result))]









