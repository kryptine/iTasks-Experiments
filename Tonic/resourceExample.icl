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

myExamples :: [Workflow]
myExamples = 	[	workflow "resource" 				"resource"  tryout
			 	]

myResources :: Shared [Resource MyResources] 
myResources = sharedStore "myResources" initResources
where
	initResources = [{name = Cup, 			kind = Reusable, 	available = 2	, inUse = []}
					,{name = Water, 		kind = Consumable, 	available = 50	, inUse = []}
					,{name = TeaBag, 		kind = Consumable, 	available = 10	, inUse = []}
					,{name = CoffeeBeans, 	kind = Consumable, 	available = 100	, inUse = []}
					,{name = Sugar, 		kind = Consumable, 	available = 40	, inUse = []}
					,{name = Milk, 			kind = Consumable, 	available = 100	, inUse = []}
					]	

// test
coffee		= {id = CoffeeBeans, demanded = 10}
water	 	= {id = Water,		 demanded = 5}
water2	 	= {id = Water,		 demanded = 10}
cup			= {id = Cup,		 demanded = 1}
tea			= {id = TeaBag,		 demanded = 1}

wantCoffee = (([coffee,water,cup],myResources),("drink coffee",showIt "coffee request" "coffee"))
wantTea	   = (([tea,water2,cup],myResources),("drink tea",showIt "tea request" "tea"))

showIt ask result = viewInformation ask [] "" >>* [OnAction ActionOk (always (return result))]

:: RTask r a :== (([Requested r],Shared [Resource r]),(String,Task a))

//(-&?&-) infixr 3 	:: !(RTask r a) !(RTask r a) 	-> RTask r a 				| iTask a & iTask r
(-&?&-) (r1,ta1) (r2,ta2) = ((sumRequested (fst r1) (fst r2),snd r1),("-&&-",snd ta1 -&&- snd ta2))

(-|?|-) infixr 3 	:: !(RTask r a) !(RTask r a) 	-> RTask r a 				| iTask a & iTask r
(-|?|-) (r1,ta1) (r2,ta2) = ((maxRequested (fst r1) (fst r2),snd r1),("-||-",snd ta1 -||- snd ta2))

appl ::(RTask r a) -> Task a | iTask a & iTask r
appl (reqres,task) = reqres ?: task

tryout :: Task ()
tryout 
	=	(	forever (appl wantCoffee) -&&-
			forever (appl wantTea) -&&-
			forever (appl (wantCoffee -|?|- wantTea)) -&&-
			forever (appl (wantCoffee -&?&- wantTea)) -&&-
			showResources myResources -&&-
			alterResources myResources
		)
		@ const ()





