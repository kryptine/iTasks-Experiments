module ClientExecution

import iTasks.Framework.Client.RunOnClient
import Text
import StdMisc

bae 		:== "Basic API Examples"
basicTypes	:== bae +++ "/Interaction with basic types/"
costumTypes :== bae +++ "/Interaction with custom types/"
sharedData	:== bae +++ "/Interaction with shared data/"
seqTasks	:== bae +++ "/Sequential task composition/"
parTasks	:== bae +++ "/Parallel task composition/"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow (basicTypes +++ "Hello world") 			 	"View a constant string" 			(runOnClient helloWorld)
	,workflow (basicTypes +++ "Enter a string") 		 	"Entering a string" 				(runOnClient enterString)
	,workflow (basicTypes +++ "Enter an integer") 		 	"Entering an integer" 				(runOnClient enterInt)
	,workflow (basicTypes +++ "Enter a date & time") 	 	"Entering a date & time" 			(runOnClient enterDateTime)

	,workflow (costumTypes +++ "Enter a person") 		 	"Entering a person" 				(runOnClient enterPerson)
	,workflow (costumTypes +++ "Enter multiple persons") 	"Entering multiple persons" 		(runOnClient enterPersons)
	,workflow (costumTypes +++ "View a person")             "View a person"                     (runOnClient viewPerson)
	
	,workflow (sharedData +++ "View date and time")		 	"View the current date and time" 	(runOnClient viewCurDateTime)
	,workflow (sharedData +++ "Edit stored persons") 	 	"Update a stored list of persons" 	(runOnClient editStoredPersons)
	,workflow (sharedData +++ "View stored persons") 	 	"View a stored list of persons" 	(runOnClient viewStoredPersons)
	,workflow (sharedData +++ "Editors on shared note") 	"edit notes" 						notes
	,workflow (sharedData +++ "Edit note or List of strings") "Edit note or List of strings" 	(runOnClient linesPar)
	
	,workflow (seqTasks +++ "Hello User") 	 			 	"Enter your name:" 					(runOnClient hello)
	,workflow (seqTasks +++ "Positive Number") 	 			"Enter a positive number:"			(runOnClient positiveNumber)
	,workflow (seqTasks +++ "Palindrome") 	 			 	"Enter a Palindrome" 				(runOnClient palindrome)
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers" 				(runOnClient calculateSum)
	,workflow (seqTasks +++ "Sum, with backstep") 	 		"Sum, with backstep" 				(runOnClient calculateSumSteps)
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers 2" 				(runOnClient calculateSum2)
	,workflow (seqTasks +++ "Add persons 1 by 1") 	 		"Add persons 1 by 1" 				(runOnClient (person1by1 []))
	
	,workflow (seqTasks +++ "Coffee Machine") 	 			"Coffee Machine" 					(runOnClient coffeemachine)
	,workflow (seqTasks +++ "Calculator") 	 				"Calculator" 						(runOnClient calculator)
	,workflow (seqTasks +++ "Edit shared list of persons") 	"Edit shared list of persons" 		(runOnClient editPersonList)
	,workflow (seqTasks +++ "Edit shared todo list") 		"Edit shared todo list" 			(runOnClient editToDoList)
	,workflow (seqTasks +++ "Follow tweets of a user") 		"Follow tweets of a user" 			(runOnClient followTweets)

	,workflow (parTasks +++ "Simple editor with statistics")"Edit text" 						(runOnClient editWithStatistics)
	]

//* utility functions

always t = const (Just t)

hasValue  tf (Value v _) = Just (tf v)
hasValue _ _ = Nothing

getValue (Value v _) = v

ifValue pred tf (Value v _) = if (pred v) (Just (tf v)) Nothing
ifValue _ _ _ = Nothing

ifStable (Value v stable) = stable
ifStable _ = False

returnF :: (a -> b) (TaskValue a) -> Maybe (Task b) | iTask b
returnF fun (Value v _) = Just (return (fun v))
returnF _ _				= Nothing

returnV :: (TaskValue a) -> Maybe (Task a) | iTask a
returnV (Value v _) = Just (return v)
returnV _			= Nothing

returnP :: (a -> Bool) (TaskValue a) -> Maybe (Task a) | iTask a
returnP pred (Value v _)
	| pred v	= Just (return v)
				= Nothing
returnP _ _		= Nothing

getUserName :: User -> String
getUserName u = toString u

toMaybe :: (TaskValue a) -> Maybe a
toMaybe (Value v _) =  (Just v)
toMaybe _   =  Nothing

//* Basic interaction

helloWorld :: Task String
helloWorld = viewInformation "You have a message from iTasks:" [] "Hello world!" 

enterString :: Task String
enterString = enterInformation "Enter a string" []

enterInt :: Task Int
enterInt = enterInformation "Enter an integer" []

enterDateTime :: Task DateTime
enterDateTime = enterInformation "Enter a date and time" []

viewIntList :: Task [Int]
viewIntList = viewInformation "View the numbers from 1 to 10" [] [1..10]

//* Interaction using user-defined types

:: MyPerson =
	{ name			:: String
	, gender		:: MyGender
	, dateOfBirth	:: Maybe Date
	}
	
:: MyGender = Male | Female

derive class iTask MyPerson, MyGender

enterPerson :: Task MyPerson 
enterPerson = enterInformation "Enter your personal information" [EnterWith (\(n, g) -> {MyPerson | name=n, gender=g, dateOfBirth=Nothing})]

enterPersons :: Task [MyPerson]
enterPersons = enterInformation "Enter personal information of multiple people" []

viewPerson :: Task MyPerson
viewPerson = viewInformation "View a person" [ViewWith (\{MyPerson | name,gender} -> (name,gender))] {name = "Peter Achten", gender = Male,dateOfBirth = Nothing}

//* Interaction with shared data

viewCurDateTime :: Task DateTime
viewCurDateTime = viewSharedInformation "The current date and time is:" [] currentDateTime

personStore :: Shared [MyPerson]
personStore = sharedStore "Persons" []

editStoredPersons :: Task [MyPerson]
editStoredPersons = updateSharedInformation "Update the stored list of persons" [] personStore

viewStoredPersons :: Task [MyPerson] 
viewStoredPersons = viewSharedInformation "These are the currently stored persons" [] personStore

notes :: Task Note
notes 
	= runOnClient (withShared (Note "") (\note -> exposeShared note body))
where
	body _ note 
		= //runOnClient (
		 	viewSharedInformation "view on note" [] note
			-||-
			updateSharedInformation "edit shared note 1" [] note
			-||-
			updateSharedInformation "edit shared note 2" [] note
		//)
		
linesPar :: Task (Maybe String)
linesPar
	=	withShared "" doEditor
where
	doEditor state
		= 			noteE state 
					-||- 
					lineE state
			>>* 	[OnAction ActionQuit (Just o return o toMaybe)]

	noteE state 
		= 			updateSharedInformation ("Text","Edit text") [noteEditor] state
			>>*		[ OnAction (Action "Trim" []) (\txt -> Just (upd trim state >>| noteE state))	
					]

	lineE state
		=	updateSharedInformation ("Lines","Edit lines") [listEditor] state

	noteEditor = UpdateWith (\txt -> Note txt) (\_ (Note txt) -> txt)
	listEditor = UpdateWith (split "\n") (\_ l -> join "\n" l)

//* Sequential task composition

hello :: Task String
hello 
	=           enterInformation "Please enter your name" []
        >>= 	viewInformation ("Hello ") [] 

positiveNumber :: Task Int
positiveNumber 
	= 		enterInformation "Please enter a positive number" []
		>>* [ OnAction  ActionOk (returnP (\n -> n >= 0))
            ] 

palindrome :: Task (Maybe String)
palindrome 
	=   	enterInformation "Enter a palindrome" []
		>>* [ OnAction  ActionOk     (ifValue palindrome (\v -> return (Just v)))
            , OnAction  ActionCancel (always (return Nothing))
            ]
where
	palindrome s = lc == reverse lc
	where lc :: [Char]
		  lc = fromString s

person1by1 :: [MyPerson] -> Task [MyPerson]
person1by1 persons
	=       enterInformation "Add a person" [] 	//-|| viewInformation "List so far.." [] persons
		>>*	[ OnAction  (Action "Add" []) 		(hasValue (\v -> person1by1  [v : persons]))
		    , OnAction  (Action "Finish" [])    (always (return persons))
		    , OnAction  ActionCancel 			(always (return []))
	        ]

// BUG? not always all record fields are shown in a choice...
// sometimes I get several continues... does not looks nice

editPersonList :: Task Void
editPersonList = editSharedList personStore

editSharedList :: (Shared [a]) -> Task Void | iTask a
editSharedList store
	=			enterChoiceWithShared "Choose an item to edit" [ChooseWith (ChooseFromGrid snd)] (mapRead (\ps -> [(i,p) \\ p <- ps & i <- [0..]]) store)
		>>*		[ OnAction (Action "Append" [])   (hasValue (showAndDo append))
				, OnAction (Action "Delete" [])   (hasValue (showAndDo delete))
				, OnAction (Action "Edit" [])     (hasValue (showAndDo edit))
				, OnAction (Action "Clear" [])    (always (showAndDo append (-1,undef)))
				, OnAction (Action "Quit" [])     (always (return Void))
				]
where
	showAndDo fun ip
		=		viewSharedInformation "In store" [] store
 		 		||- 
 		 		fun ip
 		 	>>* [ OnValue 					    (hasValue	(\_ -> editSharedList store))
 		 		, OnAction (Action "Cancel" []) (always	(editSharedList store))
 		 		]

	append (i,_)
		=			enterInformation "Add new item" []
		>>=	\n ->	upd (\ps -> let (begin,end) = splitAt (i+1) ps in (begin ++ [n] ++ end)) store
	delete (i,_)
		=			upd (\ps -> removeAt i ps) store
	edit (i,p)
		=			updateInformation "Edit item" [] p 
		 >>= \p -> 	upd (\ps ->  updateAt i p ps) store

//

:: ToDo =	{ name     :: String
			, deadline :: Maybe Date
			, remark   :: Maybe Note
			, done     :: Bool
			}
derive class iTask ToDo

toDoList :: Shared [ToDo]
toDoList = sharedStore "My To Do List" []

editToDoList = editSharedList toDoList

//* tweets

:: Tweet  :== (String,String)

twitterId :: String -> Shared [Tweet]
twitterId name  = sharedStore ("Twitter with " +++ name) []

followTweets 
	= 					get currentUser
		>>= \me ->		enterChoiceWithShared "Whoms tweets you want to see?" [] users
		>>= \user ->	let name = getUserName user in joinTweets me user "type in your tweet" (twitterId name)
where
	joinTweets  :: User User String (Shared [Tweet]) -> Task Void
	joinTweets me you message tweetsStore
		=			(viewSharedInformation ("You are following " +++ tweeter) [] tweetsStore)
					||-
					(you @: tweeting)
	where
		tweeter = getUserName you

		tweeting 
			=			updateInformation "Add a tweet" [] message
						-||
						viewSharedInformation ("Tweets of " +++ tweeter) [] tweetsStore
				>>*		[ OnAction (Action "Quit" [])    (always (return Void))
						, OnAction (Action "Commit" [])  (hasValue commit )
						]

		commit :: String -> Task Void
		commit message
			=				upd (\tweets -> [(tweeter,message)] ++ tweets) tweetsStore 
				>>| 		tweeting 

//

calculateSum :: Task Int
calculateSum
  =   enterInformation ("Number 1","Enter a number") []
  >>= \num1 ->
      enterInformation ("Number 2","Enter another number") []
  >>= \num2 ->
      viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)

calculateSumSteps :: Task Int
calculateSumSteps = step1 0 0
where
	step1 n1 n2		=		updateInformation ("Number 1","Enter first number")  [] n1
						>>*	[ OnAction ActionNext (hasValue (\n1 -> step2 n1 n2))
							]
	step2 n1 n2		=		updateInformation ("Number 2","Enter second number") [] n2
						>>*	[ OnAction ActionPrevious (always 	(step1 n1 n2))
							, OnAction ActionNext     (hasValue (\n2 -> step3 n1 n2))]
	step3 n1 n2		=		viewInformation ("Sum","The sum of those numbers is:") [] (n1 + n2)
						>>*	[ OnAction ActionPrevious	(always 	(step2 n1 n2))
						  	, OnAction ActionOk  		(always  	(return (n1 + n2)))
						  	]
//
:: MySum = {firstNumber :: Int, secondNumber :: Int, sum :: Display Int}
derive class iTask MySum

calculateSum2 :: Task Int
calculateSum2
  = 				updateInformation ("Sum of 2 numbers, with view","") 
  						[UpdateWith (\(i,j) -> {firstNumber = i, secondNumber = j, sum = Display (i+j)}) 
  						            (\_ res -> (res.firstNumber,res.secondNumber))] (0,0)
  	>>= \(i,j) -> 	return (i+j)

//
coffeemachine :: Task (String,EUR)
coffeemachine  
	=	enterChoice ("Product","Choose your product:") []
					[("Coffee", EUR 100)
					,("Cappucino", EUR 150)
					,("Tea", EUR 50)
					,("Chocolate", EUR 100)
					] 
	>>=  getCoins (EUR 0)
	>>|  coffeemachine

getCoins :: EUR (String,EUR) -> Task (String,EUR)
getCoins paid (product,toPay) 
	= 				viewInformation "Coffee Machine" [ViewWith view1] toPay
					||-		
					enterChoice  ("Insert coins","Please insert a coin...") [ChooseWith (ChooseFromRadioButtons id)] coins
			>>*		[ OnAction ActionCancel 		(always (stop ("Cancelled",paid)))
					, OnAction (Action "Insert" []) (hasValue handleMoney)
					]
where				
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	handleMoney coin 
	| toPay > coin	= getCoins (paid+coin) (product, toPay-coin)
	| otherwise		= stop (product,coin-toPay)
	
	stop (product, money) = viewInformation "Coffee Machine" [ViewWith view2] (product,money)

	view1 toPay 		   = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("To pay: " <+++ toPay)])]
	view2 (product,money)  = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("Money returned: " <+++ money)])]

// BUG? needs more work on lay-out and should work on reals to allow dividing...

:: CalculatorState = { display :: Int, n :: Int }

derive class iTask CalculatorState

calculator :: Task Int
calculator = calc initSt
where
	calc st
	= 		viewInformation "Calculator" [ViewWith Display] st
		>>* [ OnAction (Action "7" []) (always (updateDigit 7 st)) 
			, OnAction (Action "8" []) (always (updateDigit 8 st))
			, OnAction (Action "9" []) (always (updateDigit 9 st))
			, OnAction (Action "4" []) (always (updateDigit 4 st)) 
			, OnAction (Action "5" []) (always (updateDigit 5 st))
			, OnAction (Action "6" []) (always (updateDigit 6 st))
			, OnAction (Action "1" []) (always (updateDigit 1 st)) 
			, OnAction (Action "2" []) (always (updateDigit 2 st))
			, OnAction (Action "3" []) (always (updateDigit 3 st)) 
			, OnAction (Action "0" []) (always (updateDigit 0 st))
			, OnAction (Action "+" []) (always (apply (+) st))
			, OnAction (Action "-" []) (always (apply (-) st))
			, OnAction (Action "*" []) (always (apply (*) st))
			, OnAction (Action "/" []) (always (apply (/) st))
			]
	where
		updateDigit n st = calc {st & n = st.n*10 + n}
	
		apply op st = calc {display = op st.display st.n, n = 0}

	initSt = { display = 0, n = 0}

//* Parallel task composition

derive class iTask Statistics, Replace

:: Statistics = {lineCount :: Int
				,wordCount :: Int
				}
:: Replace	 =	{ search  :: String
				, replaceBy :: String
				}

initReplace =	{ search = ""
				, replaceBy = "" 
				}
stat text = {lineCount = lengthLines text, wordCount = lengthWords text}
where
	lengthLines ""   = 0
	lengthLines text = length (split "\n" text)

	lengthWords "" 	 = 0
	lengthWords text = length (split " " (replaceSubString "\n" " " text))

editWithStatistics :: Task Void
editWithStatistics 
 =						enterInformation "Give name of text file you want to edit..." []
	>>= \fileName -> 	let file = sharedStore fileName ""
						in	parallel Void 	[ (Embedded, showStatistics file )
									  		, (Embedded, editFile fileName file)
									  		, (Embedded, replace initReplace file)
									  		] []
							>>*	 			[ OnAction (ActionQuit) (always (return Void))
											]
						
editFile :: String (Shared String) (SharedTaskList Void) -> Task Void
editFile fileName sharedFile _
 =						updateSharedInformation ("edit " +++ fileName) [UpdateWith toV fromV] sharedFile
 	@ 					const Void
where
	toV text 			= Note text
	fromV _ (Note text) = text

showStatistics sharedFile _  = noStat <<@ InWindow
where
	noStat :: Task Void
	noStat	=			viewInformation Void [] Void
 				>>*		[ OnAction (Action "/File/Show Statistics" []) (always showStat)
 						]
	showStat :: Task Void 
	showStat =			viewSharedInformation "Statistics:" [ViewWith stat] sharedFile 
 				>>*		[ OnAction (Action "/File/Hide Statistics" []) (always noStat)
 						]


replace cmnd sharedFile _ = noReplace cmnd <<@ InWindow
where
	noReplace :: Replace -> Task Void
	noReplace cmnd 
		=		viewInformation Void [] Void
 			>>*	[ OnAction (Action "/File/Replace" []) (always (showReplace cmnd))
				]

	showReplace :: Replace -> Task Void 
	showReplace cmnd
		=		updateInformation "Replace:" [] cmnd 
 			>>*	[ OnAction (Action "Replace" []) (hasValue substitute)
 				, OnAction (Action "Cancel" [])  (always (noReplace cmnd))
 				]
 			
 	substitute cmnd =	upd (replaceSubString cmnd.search cmnd.replaceBy) sharedFile 
 						>>| showReplace cmnd

Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist basicAPIExamples)) world


