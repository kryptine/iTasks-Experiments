module resourceExample

import resource


import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "CEFP 2015 iTasks & Tonic examples" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
        ] world
 
:: MyResources	=	Cup | Water | CoffeeBeans | TeaBag | Sugar | Milk 

derive class iTask MyResources

myResources :: Shared [Resource MyResources] 
myResources = sharedStore "myResources" initResources
where
	initResources = [{name = Cup, 			kind = Reusable, 	available = 1	, inUse = []}
					,{name = Water, 		kind = Consumable, 	available = 15	, inUse = []}
					,{name = TeaBag, 		kind = Consumable, 	available = 10	, inUse = []}
					,{name = CoffeeBeans, 	kind = Consumable, 	available = 50	, inUse = []}
					,{name = Sugar, 		kind = Consumable, 	available = 10	, inUse = []}
					,{name = Milk, 			kind = Consumable, 	available = 10	, inUse = []}
					]	


myExamples :: [Workflow]
myExamples = 	[	workflow "resource" 				"resource"  test
			 	]



wantStarBucks :: (Resource MyResources) -> Amount 
wantStarBucks {Resource|name = Cup} = 1
wantStarBucks {Resource|name = Water} = 5
wantStarBucks {Resource|name = CoffeeBeans} = 10
wantStarBucks _ = 0

wantDE :: (Resource MyResources) -> Amount 
wantDE {Resource|name = Cup} = 1
wantDE {Resource|name = Water} = 5
wantDE {Resource|name = TeaBag} = 5
wantDE {Resource|name = Sugar} = 2
wantDE {Resource|name = Milk} = 2
wantDE _ = 0
		
test = forever (((wantStarBucks,id),myResources)	?: ("drink coffee",showIt "coffee request" "coffee")) -&&-
	   forever (((wantDE,id),myResources)			?: ("drink tea",showIt "tea request" "tea")) -&&-
	   showResources myResources -&&-
	   alterResources myResources

showIt ask result = viewInformation ask [] "" >>* [OnAction ActionOk (always (return result))]

