module BasicAPIExamples
import iTasks
import iTasks.API.Extensions.Admin.UserAdmin
import iTasks.API.Extensions.Admin.ServerAdmin
import iTasks.API.Extensions.Admin.StoreAdmin
import iTasks.API.Extensions.Admin.WorkflowAdmin
import iTasks.API.Extensions.GIS.GoogleMap
import Text, StdArray

import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

import qualified Data.Map as DM

/**
* This module contains a series of small examples of basic usage of the iTasks API.
*/

//* Running the tasks in a workflow browser

bae 		:== "Basic API Examples"
basicTypes	:== bae +++ "/Interaction with basic types/"
customTypes :== bae +++ "/Interaction with custom types/"
sharedData	:== bae +++ "/Interaction with shared data/"
seqTasks	:== bae +++ "/Sequential task composition/"
parTasks	:== bae +++ "/Parallel task composition/"
distrTask	:== bae +++ "/Distributed tasks/"
svgTasks    :== bae +++ "/SVG experiments/"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow (basicTypes +++ "Hello world") 			 	"View a constant string" 			helloWorld
	,workflow (basicTypes +++ "Enter a string") 		 	"Entering a string" 				enterString
	,workflow (basicTypes +++ "Enter an integer") 		 	"Entering an integer" 				enterInt
	,workflow (basicTypes +++ "Enter a date & time") 	 	"Entering a date & time" 			enterDateTime
	,workflow (basicTypes +++ "Browse Google Map") 			"Browse the map" 					browseGoogleMap

	,workflow (customTypes +++ "Enter a person") 		 	"Entering a person" 				enterPerson
	,workflow (customTypes +++ "Enter multiple persons") 	"Entering multiple persons" 		enterPersons
	,workflow (customTypes +++ "View a person")             "View a person"                     viewPerson

	,workflow (sharedData +++ "View date and time")		 	"View the current date and time" 	viewCurDateTime
	,workflow (sharedData +++ "View time")                  "View the current time"             viewTime
	,workflow (sharedData +++ "Edit stored persons") 	 	"Update a stored list of persons" 	editStoredPersons
	,workflow (sharedData +++ "View stored persons") 	 	"View a stored list of persons" 	viewStoredPersons
	,workflow (sharedData +++ "Editors on shared note") 	"edit notes" 						notes
	,workflow (sharedData +++ "Edit note or List of strings") "Edit note or List of strings" 	linesPar
	,workflow (sharedData +++ "Edit and view Google map")   "Edit and view Google map" 			browseAndViewGoogleMap

	,workflow (seqTasks +++ "Hello User") 	 			 	"Enter your name:" 					hello
	,workflow (seqTasks +++ "Positive Number") 	 			"Enter a positive number:" 			positiveNumber
	,workflow (seqTasks +++ "Palindrome") 	 			 	"Enter a Palindrome" 				palindrome
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers" 				calculateSum
	,workflow (seqTasks +++ "Sum, with backstep") 	 		"Sum, with backstep" 				calculateSumSteps
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers 2" 				calculateSum2
	,workflow (seqTasks +++ "Add persons 1 by 1") 	 		"Add persons 1 by 1" 				(person1by1 [])

	,workflow (seqTasks +++ "Coffee Machine") 	 			"Coffee Machine" 					coffeemachine
	,workflow (seqTasks +++ "Calculator") 	 				"Calculator" 						calculator
	,workflow (seqTasks +++ "Edit shared list of persons") 	"Edit shared list of persons" 		editPersonList
	,workflow (seqTasks +++ "Edit shared todo list") 		"Edit shared todo list" 			editToDoList
	,workflow (seqTasks +++ "Follow tweets of a user") 		"Follow tweets of a user" 			followTweets


	,workflow (parTasks +++ "Simple editor with statistics")"Edit text" 						editWithStatistics

	,workflow (distrTask +++ "Delegate Enter a person") 	"Delegate Enter a person" 			(delegate enterPerson)
	,workflow (distrTask +++ "Chat with someone")   		"Chat with someone" 				chat
	,workflow (distrTask +++ "Plan meeting") 				"Plan meeting" 						testMeeting
	,workflow (distrTask +++ "Tic-Tac-Toe")                 "Play tic-tac-toe"                  tictactoe

	,workflow "Droste Cacaobus" 							"Start this application as a task" 	(manageWorklist basicAPIExamples)

	,workflow "Manage users" 							 	"Manage system users..." 			manageUsers
	,workflow "Manage server" 							 	"Manage itask server..." 			manageServer
	,workflow "Manage store" 							 	"Manage itask store..." 			manageStore

	]


Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> loginAndManageWorkList "iTasks Example Collection" basicAPIExamples)
						  , publish "/persons" (WebApp []) (const enterPersons)
						  , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
 						  ] world
		
		
//* utility functions
undef = undef


getValue (Value v _) = v


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

toMaybe :: (TaskValue a) -> Maybe a
toMaybe (Value v _) =  (Just v)
toMaybe _   =  Nothing

getUserName :: User -> String
getUserName u = toString u
//getUserName (AuthenticatedUser id _ (Just name)) = name +++ id
//getUserName _ = "Anonymous"

//* The example tasks are colelcted in categories:

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

browseGoogleMap :: Task GoogleMap
browseGoogleMap = enterInformation ("Browse Map","Move around on the map. Your perspective is persistent during the task") []

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

import iTasks.API.Extensions.Clock
viewTime :: Task Time
viewTime = viewSharedInformation "The current time is:" [ViewWith AnalogClock] currentTime

personStore :: Shared [MyPerson]
personStore = sharedStore "Persons" []

editStoredPersons :: Task [MyPerson]
editStoredPersons = updateSharedInformation "Update the stored list of persons" [] personStore

viewStoredPersons :: Task [MyPerson] 
viewStoredPersons = viewSharedInformation "These are the currently stored persons" [] personStore

notes :: Task Note
notes 
	= withShared (Note "")
		(\note -> 	viewSharedInformation "view on note" [] note
					-||-
					updateSharedInformation "edit shared note 1" [] note
					-||-
					updateSharedInformation "edit shared note 2" [] note
		)

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

browseAndViewGoogleMap :: Task GoogleMap
browseAndViewGoogleMap = withShared defaultValue (\smap -> updateSharedInformation "Browse Map" [] smap 
																-|| 
														   viewSharedInformation "View Browsing Map" [] smap )

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
		>>*		[ OnAction (Action "Append" [])   (hasValue (\v -> showAndDo store append v))
				, OnAction (Action "Delete" [])   (hasValue (\v -> showAndDo store delete v))
				, OnAction (Action "Edit" [])     (hasValue (\v -> showAndDo store edit v))
				, OnAction (Action "Clear" [])    (always (showAndDo store append (-1,undef)))
				, OnAction (Action "Quit" [])     (always (return Void))
				]

showAndDo store fun ip
	=		viewSharedInformation "In store" [] store
		 		||- 
		 		fun store ip
		 	>>* [ OnValue 					    (hasValue	(\_ -> editSharedList store))
		 		, OnAction (Action "Cancel" []) (always	(editSharedList store))
		 		]

append store (i,_)
	=			enterInformation "Add new item" []
	>>=	\n ->	upd (\ps -> let (begin,end) = splitAt (i+1) ps in (begin ++ [n] ++ end)) store
delete store (i,_)
	=			upd (\ps -> removeAt i ps) store
edit store (i,p)
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
					, OnAction (Action "Insert" []) (hasValue (\v -> handleMoney paid (product,toPay) v))
					]
where				
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	view1 toPay 		   = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("To pay: " <+++ toPay)])]

handleMoney paid (product,toPay) coin 
| toPay > coin	= getCoins (paid+coin) (product, toPay-coin)
| otherwise		= stop (product,coin-toPay)

stop (product, money) = viewInformation "Coffee Machine" [ViewWith view2] (product,money)
where
	view2 (product,money)  = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("Money returned: " <+++ money)])]

// BUG? needs more work on lay-out and should work on reals to allow dividing...

:: CalculatorState = { display :: Int, n :: Int }

derive class iTask CalculatorState

calculator :: Task Int
calculator = calc initSt
where
	initSt = { display = 0, n = 0}

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
						in	parallel  	    [ (Embedded, showStatistics file )
									  		, (Embedded, editFile fileName file)
									  		, (Embedded, replace initReplace file)
									  		] []
							>>*	 			[ OnAction (ActionQuit) (always (return Void))
											]
											

editFile :: String (Shared String) (SharedTaskList Void) -> Task Void
editFile fileName sharedFile _
 =						updateSharedInformation ("edit " +++ fileName) [UpdateWith toV fromV] sharedFile
 >>|					return Void
// 	@ 					const Void					// cause Tonic to crash
where
	toV text 			= Note text
	fromV _ (Note text) = text


showStatistics sharedFile _  = noStat sharedFile <<@ InWindow

noStat sharedFile=		viewInformation Void [] Void
				>>*		[ OnAction (Action "/File/Show Statistics" []) (always (showStat sharedFile))
						]
showStat sharedFile =	viewSharedInformation "Statistics:" [ViewWith stat] sharedFile
				>>*		[ OnAction (Action "/File/Hide Statistics" []) (always (noStat sharedFile))
						]

replace cmnd sharedFile _ = noReplace sharedFile cmnd <<@ InWindow

noReplace sharedFile cmnd 
	=		viewInformation Void [] Void
			>>*	[ OnAction (Action "/File/Replace" []) (always (showReplace sharedFile cmnd))
			]

showReplace sharedFile cmnd
	=		updateInformation "Replace:" [] cmnd 
			>>*	[ OnAction (Action "Replace" []) (hasValue (\v -> substitute sharedFile v))
				, OnAction (Action "Cancel" [])  (always (noReplace sharedFile cmnd))
				]
			
substitute sharedFile cmnd =	upd (replaceSubString cmnd.search cmnd.replaceBy) sharedFile 
					>>| showReplace sharedFile cmnd

//* Distributing tasks

// delegate

delegate :: (Task a) -> Task a | iTask a
delegate task
	=					enterChoiceWithShared "Select someone to delegate the task to:" [] users
		>>= \user -> 	user @: (task >>= return)
		>>= \result ->	viewInformation "The result is:" [] result

// chat


chat :: Task Void
chat = 					get currentUser
		>>= \me ->		enterChoiceWithShared "Select someone to chat with:" [] users
		>>= \you -> 	withShared ("","") (duoChat me you)
where
	duoChat me you notes
		=	chat you toView fromView notes
			-||- 
			(you @: chat me (toView o switch) (\a v -> switch (fromView a v)) notes)

	chat who toView fromView notes
		= 			updateSharedInformation ("Chat with " <+++ who) [UpdateWith toView fromView] notes
			>>*		[OnAction (Action "Stop" []) (always (return Void))]

	toView   (me,you) 							= (Display you, Note me)
	fromView _ (Display you, Note me) 	= (me,you) 

	switch (me,you) = (you,me)

// plan meeting

testMeeting :: Task DateTime
testMeeting
	=	enterSharedMultipleChoice ("Choose users","Select the users with whom you want to plan a meeting...") [] users
	>>=	planMeeting

planMeeting :: [User] -> Task DateTime
planMeeting users =   enterDateTimeOptions
                 >>* [ OnAction (Action "Continue" []) (hasValue (\v -> ask users v))]
                 >>* [ OnAction (Action "Try again" []) (always (planMeeting users))
                     , OnAction (Action "Make decision" []) (hasValue pick)
                     ]

enterDateTimeOptions :: Task [DateTime]
enterDateTimeOptions = enterInformation "Propose meeting dates and times..." []

ask :: [User] [DateTime] -> Task [(User,[DateTime])]
ask users options
	= parallel
	  [ (Embedded, monitor) 
	  :[(Detached (worker u) False,select u options) \\ u <- users]
	  ] []
	  >>= \answers -> return [a \\ (_,Value a _) <- answers]

monitor :: ParallelTask a | iTask a
monitor = \all_results -> viewSharedInformation "Results so far" [] (mapRead tl (taskListState all_results)) @? \_ -> NoValue

select :: User [DateTime] -> ParallelTask (User,[DateTime])
select user options = \_ -> (enterMultipleChoice "Enter preferences" [] options >>= \choice -> return (user,choice))

 
decide :: TaskCont [(User,[DateTime])] (Task DateTime)
decide = OnAction (Action "Make decision" []) (hasValue pick)

pick :: [(User,[DateTime])] -> Task DateTime
pick user_dates
  =   (enterChoice "Choose date" [] (transpose user_dates) >>= \(fst,snd) -> return fst)
     -||-
      (enterInformation "Enter override" [])
  >>* [OnAction (Action "Continue" []) (hasValue (\v -> return v))]

transpose :: [(a,[b])] -> [(b,[a])] | Eq b
transpose a_bs = [(b,[a \\ (a,bs) <- a_bs | isMember b bs]) \\ b <- removeDup (flatten (map snd a_bs))]

worker :: User -> TaskAttributes
worker (AuthenticatedUser id _ _) = 'DM'.fromList [("user", id)]


// tic-tac-toe, simplistic
:: TicTacToe
	= { board   :: ![[Maybe TicTac]]
	  , player1 :: !User
	  , player2 :: !User
	  , turn    :: !Bool               // player 1 is playing
	  }
:: TicTac
	= Tic | Tac

instance ~ Bool where ~ b = not b
derive class iTask TicTacToe, TicTac
instance == TicTac where == t1 t2 = t1 === t2

emptyBoard :: [[Maybe TicTac]]
emptyBoard = repeatn 3 (repeatn 3 Nothing)

viewBoard :: !(!Int,!Int) !TicTacToe -> HtmlTag
viewBoard dimensions ttt
	= TableTag [ BorderAttr "0" ] 
	           [ tr [ td [TileTag dimensions (case cell of
												Nothing  = "empty"
												Just Tic = "cross"
												Just Tac = "circle")
						 ]
					\\ cell <- row
					] 
			    \\ row <- ttt.board
			    ]

// shorthands for HTML:
tr		= TrTag []
td		= TdTag []
text x	= TdTag [AlignAttr "center"] [Text (toString x)]

TileTag :: !(!Int,!Int) !String -> HtmlTag
TileTag (width,height) tile
	= ImgTag [SrcAttr ("/"<+++ tile <+++ ".png"),w,h]
where
	(w,h) = (WidthAttr (toString width),HeightAttr (toString height))

tictactoe :: Task String
tictactoe
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Tic-Tac-Toe with:" [] users
	  >>= \you -> playGame me you {board=emptyBoard,player1=me,player2=you,turn=True}
where
	playGame me you board 
	  =				withShared  board
	                         (\sharedGameSt ->
	                         (        (tictactoe_for_1 True  sharedGameSt)
	                                       -||-
	                          (you @: (tictactoe_for_1 False sharedGameSt))
	                         ))
	  >>|			playGame me you {board	& turn = not board.turn}	



tictactoe_for_1 :: !Bool !(Shared TicTacToe) -> Task User
tictactoe_for_1 my_turn sharedGameSt
	= (viewSharedInformation "Board:" [ViewWith (\gameSt -> viewBoard (42,42) gameSt)] sharedGameSt) ||- play
where
	play= (updateSharedInformation "Play:" [UpdateWith Hidden (\gameSt _ -> gameSt)] sharedGameSt)
	      >>* [ OnValue (ifValue game_over declare_winner)
              , OnValue (ifValue on_turn   make_a_move)
              ]

	game_over {board}
		= not (isEmpty [hd nodups \\ candidate <- rows ++ columns ++ diags
		                           , nodups    <- [removeDup candidate]
		                           | length nodups == 1 && isJust (hd nodups)
		               ])
	where
		rows	= board
		columns	= [[ board !! x !! y \\ x <- [0..2]] \\ y <- [0..2]]
		diags   = [[row !! i \\ row <- board & i <- [0..]], [row !! (2-i) \\ row <- board & i <- [0..]]]

	declare_winner gameSt
		= 	let winner = if gameSt.turn gameSt.player2 gameSt.player1 
			in 		viewInformation "And the winner is: " [] (toString winner)
				>>|	return winner
	
	on_turn gameSt=:{turn}
		= turn == my_turn
	
	make_a_move gameSt=:{board,turn}
		=              enterChoice "Choose coordinate:" [] (free_coordinates board)
		  >>= \new  -> let board`  = add_cell new turn board
		                   gameSt` = {gameSt & board = board`
		                                     , turn = ~turn
		                             }
		                in set gameSt` sharedGameSt >>| play

free_coordinates :: ![[Maybe a]] -> [(Int,Int)]
free_coordinates board
	= map fst (filter (isNothing o snd) (flatten [[((x,y),cell) \\ cell <- row & x <- [1..]] \\ row <- board & y <- [1..]]))

add_cell :: !(!Int,!Int) !Bool ![[Maybe TicTac]] -> [[Maybe TicTac]]
add_cell new turn board
	= [ [  if (new == (x,y)) (Just (if turn Tic Tac)) cell
	    \\ cell <- row & x <- [1..]
	    ]
	  \\ row <- board & y <- [1..]
	  ]


