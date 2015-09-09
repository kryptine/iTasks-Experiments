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

:: Crew		=	{	userTitle	:: UserTitle
				,	role		:: OneRole
				,	location	:: Location
				}
:: Location = 	OnShip | NotOnShip
:: OneRole	=	Sailor | Captain | ShipsCook 

derive class iTask Crew, Location, OneRole

sailorAlice		= {userTitle = "alice",	role = Sailor, 		location = OnShip}
cookAlice		= {userTitle = "alice",	role = ShipsCook, 	location = OnShip}
captainBob 		= {userTitle = "bob",	role = Captain, 	location = OnShip}
sailorCarol		= {userTitle = "carol",	role = Sailor, 		location = OnShip}
sailorDave		= {userTitle = "dave",	role = Sailor, 		location = NotOnShip}
sailorEdward	= {userTitle = "edward",role = Sailor, 		location = OnShip}

myCrew :: (Resources Crew) 					// initialize crew
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
wantSailor needed {Resource|name = {role = Sailor, location = OnShip} , available } = if (available >= needed) needed 0 // % of the sailors time
wantSailor _ _ = 0

// tasks


myExamples :: [Workflow]
myExamples = 	[ workflow "resource" 				"resource"  						test
				, workflow "Manage users" 			"Manage system users..." 			manageUsers
			 	]
		
test = forever ((wantSailor 20,myCrew) ?: ("need a sailor",showIt "sailor request 1" "sail 1")) -&&-
	   forever ((wantSailor 50,myCrew) ?: ("need a sailor",showIt "sailor request 2" "sail 2")) -&&-
	   showResources myCrew -&&-
	   alterResources myCrew

showIt ask result = viewInformation ask [] "" >>* [OnAction ActionOk (always (return result))]









