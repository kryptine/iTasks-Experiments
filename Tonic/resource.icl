module resource
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "CEFP 2015 iTasks & Tonic examples" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
        ] world
 
myExamples :: [Workflow]
myExamples = 	[	workflow "resource" 				"resource"  tryout
			 	]

:: Resource a	=	{ kind		:: ResourceKind
					, name		:: a
					, available :: Int
					}
:: ResourceKind = 	Consumable | Reusable
:: Requested a	=	{ id		:: a
				  	, demanded	:: Int
				  	}

//
:: MyResources	=	Cup | Water | CoffeeBeans | TeaBag | Sugar | Milk 


derive class iTask Resource, ResourceKind, MyResources, Requested

myResources :: Shared [Resource MyResources] 
myResources = sharedStore "myResources" initResources
where
	initResources = [{name = Cup, 			kind = Reusable, 	available = 2}
					,{name = Water, 		kind = Consumable, 	available = 50}
					,{name = TeaBag, 		kind = Consumable, 	available = 10}
					,{name = CoffeeBeans, 	kind = Consumable, 	available = 100}
					,{name = Sugar, 		kind = Consumable, 	available = 40}
					,{name = Milk, 			kind = Consumable, 	available = 100}
					]	
//
isAvailable :: [Requested a] [Resource a] -> Bool | iTask a// assumed every resource is mentioned precisely once
isAvailable  requests resources
	= and [ available >= demanded 	\\ {name,available} <- resources
									, {id,demanded} <- requests 
									| id === name ]

claimResources :: [Requested a] [Resource a] -> [Resource a] | iTask a	// assumed that it is tested that they are available
claimResources requests resources 
	= [{name = name, kind = kind, available = available - demanded}
								\\ {name,kind,available} <- resources
								, {id,demanded} <- requests 
								| id === name]
	 ++
	 [resource \\ resource <- resources | noMember resource.Resource.name (map (\{id} -> id) requests)]

noMember :: a [a] -> Bool | iTask a
noMember name [] = True
noMember name [name2:rest]
| name === name2 = False
= noMember name rest

refundResources :: [Requested a] [Resource a] -> [Resource a] |iTask a
refundResources requests resources  
	= [{kind = kind, name = name, available = case kind of 	Reusable 	-> available + demanded
															Consumable	-> available }
								\\ {name,kind,available} <- resources
								, {id,demanded} <- requests 
								| id === name
								]
	 ++
	 [resource \\ resource <- resources | noMember resource.Resource.name (map (\{id} -> id) requests)]

findResource :: (Requested a) [Resource a] -> Resource a | iTask a
findResource {id} resources = hd [resource \\ resource <- resources | resource.Resource.name === id]

undef = undef

maxRequested :: [Requested a] [Requested a] -> [Requested a] | iTask a
maxRequested r1 r2 = sortMax (r1 ++ r2) 
where
	sortMax [] 						= []
	sortMax [{id,demanded}:reqs]	= [{id=id,demanded = maxOf demanded same}] ++ sortMax others
	where
		same = [req.demanded \\ req <- reqs | id === req.id]

		maxOf v [] = v
		maxOf v [vv:vvs] = maxOf (if (v>vv) v vv) vvs

		others = [req \\ req <- reqs | id =!= req.id]
		
sumRequested :: [Requested a] [Requested a] -> [Requested a] | iTask a
sumRequested r1 r2 = sumAll (r1 ++ r2) 
where
	sumAll [] 						= []
	sumAll [{id,demanded}:reqs]	= [{id=id,demanded = foldl (+) demanded same}] ++ sumAll others
	where
		same = [req.demanded \\ req <- reqs | id === req.id]

		others = [req \\ req <- reqs | id =!= req.id]


mkResourceTable :: [Requested a] [Resource a] -> Table | iTask a
mkResourceTable requested res
	 = Table mkHeader tags Nothing
where
	mkHeader  			= ["Resource Name","Kind", "Enough Available?",  "Offered", "Demanded"]
	(Table _ tags _)	= toTable [(id, toSingleLineText (findResource req res).kind,isAvailable [req] res,(findResource req res).available, demanded) \\ req=:{id,demanded} <- requested]



(?:) infix 3 ::  ([Requested r],Shared [Resource r]) (String,Task a) -> Task a | iTask a & iTask r
(?:) (requested,resource) (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (mkResourceTable requested) ] resource
	>>*		[OnAction (Action "OK" /* prompt */[]) 
				(ifValue (\resources -> isAvailable requested resources) 
						 (\_ 		->					upd (claimResources requested) resource
										>>|				task
										>>= \result ->	upd (refundResources requested) resource
										>>|				viewInformation "Task Returned:" [] result
										>>|				return result
										))
			]

:: RTask r a :== (([Requested r],Shared [Resource r]),(String,Task a))

//(-&?&-) infixr 3 	:: !(RTask r a) !(RTask r a) 	-> RTask r a 				| iTask a & iTask r
(-&?&-) (r1,ta1) (r2,ta2) = ((sumRequested (fst r1) (fst r2),snd r1),("-&&-",snd ta1 -&&- snd ta2))

(-|?|-) infixr 3 	:: !(RTask r a) !(RTask r a) 	-> RTask r a 				| iTask a & iTask r
(-|?|-) (r1,ta1) (r2,ta2) = ((maxRequested (fst r1) (fst r2),snd r1),("-||-",snd ta1 -||- snd ta2))

appl ::(RTask r a) -> Task a | iTask a & iTask r
appl (reqres,task) = reqres ?: task

showIt ask result = viewInformation ask [] "" >>* [OnAction ActionOk (always (return result))]

// test
coffee		= {id = CoffeeBeans, demanded = 10}
water	 	= {id = Water,		 demanded = 5}
water2	 	= {id = Water,		 demanded = 10}
cup			= {id = Cup,		 demanded = 1}
tea			= {id = TeaBag,		 demanded = 1}

wantCoffee = (([coffee,water,cup],myResources),("drink coffee",showIt "coffee request" "coffee"))
wantTea	   = (([tea,water2,cup],myResources),("drink tea",showIt "tea request" "tea"))

tryout :: Task ()
tryout 
	=	(	forever (appl wantCoffee) -&&-
			forever (appl wantTea) -&&-
			forever (appl (wantCoffee -|?|- wantTea)) -&&-
			forever (appl (wantCoffee -&?&- wantTea)) -&&-
			showResources myResources
		)
		@ const ()

showResources :: (Shared [Resource r]) ->  Task [Resource r] | iTask r
showResources resources
	=	viewSharedInformation "Available Resources" [] resources

alterResources :: (Shared [Resource r]) ->  Task [Resource r] | iTask r
alterResources resources
	=	updateSharedInformation "Alter Resources" [] resources



/* alternative with an if ...
(?:) infix 3 ::  [Requested] (String,Task a) -> Task () | iTask a
(?:) requested (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (\res -> [(isAvailable [req] res,req) \\ req <- requested]) ] myResources
	>>= \resources ->		if (isAvailable requested resources) 
									(				upd (claimResources requested) myResources
									>>|				task
									>>= \result ->	upd (refundResources requested) myResources
									>>|				viewInformation "Task Returned:" [] result
									>>|				return ()
									)
									(viewInformation "Cannot perform task due to lack of resources" [] ())

*/
