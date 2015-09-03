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

:: Resource 	=	{ kind		:: ResourceKind
					, name		:: ResourceName
					, available	:: Int
					}
:: ResourceKind = 	Consumable | Reusable
:: Requested		:== (ResourceName,Int)
:: ResourceName	=	Kopje | Water | TheeZakjes | KoffieBonen | Suiker | Melk 
	
derive class iTask Resource, ResourceKind, ResourceName

myResources :: Shared [Resource]
myResources = sharedStore "myResources" initResources
where
	initResources = [{name = Kopje, 		kind = Reusable, 	available = 1}
					,{name = Water, 		kind = Consumable, 	available = 50}
					,{name = TheeZakjes, 	kind = Consumable, 	available = 10}
					,{name = KoffieBonen, 	kind = Consumable, 	available = 100}
					,{name = Suiker, 		kind = Consumable, 	available = 40}
					,{name = Melk, 			kind = Consumable, 	available = 100}
					]	

isAvailable :: [Requested] [Resource] -> Bool // assumed every resource is mentioned precisely once
isAvailable  requests resources
	= and [ available >= demand \\ {name,available} <- resources
								, (sort,demand) <- requests 
								| sort === name ]

claimResources :: [Requested] [Resource] -> [Resource] // assumed that it is tested that they are available
claimResources requests resources 
	= [{name = name, kind = kind, available = available - requested}
								\\ {name,kind,available} <- resources
								, (sort,requested) <- requests 
								| sort === name]
	 ++
	 [resource \\ resource <- resources | noMember resource.Resource.name (map fst requests)]

noMember :: ResourceName [ResourceName] -> Bool
noMember name [] = True
noMember name [name2:rest]
| name === name2 = False
= noMember name rest

refundResources :: [Requested] [Resource] -> [Resource] 
refundResources requests resources  
	= [{kind = kind, name = name, available = case kind of 	Reusable 	-> available + returned
															Consumable	-> available }
								\\ {name,kind,available} <- resources
								, (sort,returned) <- requests 
								| sort === name
								]
	 ++
	 [resource \\ resource <- resources | noMember resource.Resource.name (map fst requests)]


(?:) infix 3 ::  [Requested] (String,Task a) -> Task () | iTask a
(?:) requested (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (\res -> [(isAvailable [req] res,req) \\ req <- requested]) ] myResources
	>>*		[OnAction (Action "OK" /* prompt */[]) 
				(ifValue (\resources -> isAvailable requested resources) 
						 (\resource ->					upd (claimResources requested) myResources
										>>|				task
										>>= \result ->	upd (refundResources requested) myResources
										>>|				viewInformation "Task Returned:" [] result
										>>|				return ()
										))
			]
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

tryout :: Task ()
tryout 
	=	(	forever ([(KoffieBonen,10),(Water,10),(Kopje,1)] ?: ("drink Koffie",viewInformation "drink ze" [] "kopje  koffie")) -&&-
			forever ([(Water,10),(Kopje,1),(TheeZakjes,1)] ?: ("drink Thee",viewInformation "drink ze" [] "kopje thee")) -&&-
			showResources
		)
		@ const ()

showResources ::  Task [Resource]
showResources 
	=	viewSharedInformation "Available Resources" [] myResources

alterResources ::  Task [Resource]
alterResources 
	=	updateSharedInformation "Alter Resources" [] myResources

