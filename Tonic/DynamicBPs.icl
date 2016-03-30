module DynamicBPs

import MultiUser
import iTasks
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

import StdArray

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> futureConditionals)
                          , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                          , publish "/dynamicBPs" (WebApp []) (\_-> dynamicBPs1 (viewStep 42))
                          , publish "/test" (WebApp []) (\_-> test 5)	

                          , publish "/view" (WebApp []) (\_-> viewStep 5)	
                          , publish "/seq" (WebApp []) (\_-> seqTest 5)	
                          , publish "/or" (WebApp []) (\_-> orTest )	
                          , publish "/and" (WebApp []) (\_-> andTest )	
                          , publish "/andor" (WebApp []) (\_-> andOrTest )	
                          , publish "/any" (WebApp []) (\_-> anyTest )	
                          , publish "/anyall" (WebApp []) (\_-> anyAllTest 4 )	
                          , publish "/highin" (WebApp []) (\_-> highInTest seqTest )	
                          , publish "/highout" (WebApp []) (\_-> highOutTest seqTest)	
                          , publish "/highinout" (WebApp []) (\_-> highInOutTest )	

                          , publish "/updown" (WebApp []) (\_-> shareTest )	

                          , publish "/step" (WebApp []) (\_-> stepTest 5)	

                          , publish "/myTask" (WebApp []) (\_-> myTask)	
                          , publish "/myTask2" (WebApp []) (\_-> myTask2)	
                          , publish "/myTask3" (WebApp []) (\_-> myTask3)	
                          , publish "/palindrome" (WebApp []) (\_-> palindrome)	
                          , publish "/1by1" 	(WebApp []) (\_-> person1by1 [])
                          , publish "/bob" 		(WebApp []) (\_-> workAs (AuthenticatedUser "bob" [] Nothing)   doMyWork)
                          , publish "/alice"	(WebApp []) (\_-> workAs (AuthenticatedUser "alice" [] Nothing) doMyWork)
                          , publish "/twoTasksTest" (WebApp []) (\_-> twoTasksTest)	
                          , publish "/bikes" (WebApp []) (\_-> bikes)	
                          , publish "/bikes2" (WebApp []) (\_-> bikes2)	


                          ] world

doMyWork :: Task Void 
doMyWork
	=					enterChoiceWithShared "choose a task to do..." [] processesForCurrentUser
	>>= \task -> 		workOn task.TaskListItem.taskId >>| doMyWork
	
	
viewStep :: Int -> Task Int
viewStep n = viewInformation ("Step " +++ toString n) [] n

seqTest :: Int -> Task Int
seqTest n 
| n>0 = updateInformation (toString n) [] n >>= \n -> seqTest (n-1)
| otherwise = return n

orTest :: Task Int
orTest = seqTest 3 -||- seqTest 2

andTest :: Task Int
andTest = seqTest 3 -&&- seqTest 2 >>= \(v1,v2) -> return (v1 + v2)

andOrTest :: Task Int
andOrTest = andTest -||- andTest

anyTest :: Task Int
anyTest = anyTask [viewStep 1, viewStep 2, viewStep 3]

anyAllTest :: Int -> Task Int
anyAllTest n 
	=			allTasks [updateInformation ("task " +++ toString n) [] i \\ i <- [0..n] ] 
	>>= \list -> 	anyTask [viewInformation ("task " +++ toString n) [] i \\ i <-list]

highInTest :: (Int -> Task Int) -> Task Int
highInTest ifun
	=			enterInformation "enter a number:" []
	>>= \i ->	if (i > 0) (ifun i) (return i)

highOutTest :: (Int -> Task Int) -> Task Int
highOutTest itask 
	= 				enterInformation "enter a number:" []
	>>= \n ->		itask n
	>>= \m ->		itask m
	>>= \o -> 		return (itask (n+m+o))
	>>= \task -> 	task

highInOutTest :: Task Int
highInOutTest 
	= 				highInOut [viewStep 7, seqTest 2, andOrTest, anyTest, anyAllTest 3, highInOutTest] 
	>>= \task ->	task

highInOut:: [Task Int] -> Task (Task Int)
highInOut funs
	=			enterChoice "choose a task" [ChooseWith (AutoChoice (\t -> toSingleLineText t))] funs
	>>= \fun ->	return fun
 
stepTest :: Int -> Task Int
stepTest n
    =            updateInformation "step test" [] n
    >>*         [ OnAction ActionYes (always (stepTest n))                               
                , OnAction ActionNo  (always (stepTest n))
                , OnAction (Action "Pos" []) (ifValue ((<=) 0) stepTest)
                , OnAction (Action "Neg" []) (ifValue ((>=) 0) (\n -> stepTest n))
                , OnAction (Action "Stable" []) (ifStable         stepTest)
                , OnAction (Action "Unstable" []) (ifUnstable         stepTest)
                , OnAction (Action "Cond" []) (ifCond (n>10)   (return n))
				, OnValue  (ifCond (n>1000) (return n))
				, OnException  (\n -> return n)
                ]

enterInt n
	= updateInformation "enter an integer value" [] n

shareTest :: Task Int
shareTest
	=		withShared 0 (\s -> up s -||- down s)
where
	up s 	=  viewSharedInformation "up" [] s >>= \n -> set (n+1) s >>| up s
	down s 	=  viewSharedInformation "down" [] s >>= \n -> set (n-1) s >>| down s





test :: Int -> Task Int
test n
	= do (enterInt n) >>= \number -> allTasks [do (enterInt n) \\ n <- [1..number]] >>= \s -> return (sum s) 

do :: (Task a) -> Task a | iTask a
do task
 =				task
 >>= \res ->	viewInformation "result is:" [] res
 >>|			return res


simplefutureConditionals :: Int -> Task String
simplefutureConditionals num
  =   viewInformation "You have entered" [] num
  >>| if (num < 42)
        (viewInformation "Not so high" [] (toString num +++ " is less than 42"))
        (viewInformation "It's alright" [] (toString num +++ " is greater than or equal to 42"))

futureConditionals :: Task String
futureConditionals
  =           enterInformation "Enter a number" []
  >>= \num -> viewInformation "You have entered" [] num
  >>|         if (num < 42)
                (viewInformation "Not so high" [] (toString num +++ " is less than 42"))
                (viewInformation "It's alright" [] (toString num +++ " is greater than or equal to 42"))

/**
 * FOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
 */
/// FOOOOOOOOOOOOOOOOBAARRRR
dynamicBPs1 :: (Task Int) -> Task Int
dynamicBPs1 vs42
  =   viewStep 1
  //>>| if True (viewStep 14) (viewStep 15)
  //>>| viewStep 10
  >>| (      viewStep 11
       -&&- (viewStep 12 >>| viewStep 13))
  >>| viewStep 2
  >>| vs42
  >>| viewStep 3
  >>| allTasks restOfSteps
  >>| viewStep 8
  >>| vs42
  >>| viewStep 9
  where
  restOfSteps = map viewStep [4, 5, 6, 7]
  
// the following examples are used in the CEFP sheets

:: Person 	=  { name :: String, gender :: Gender, dateOfBirth :: Date}
:: Gender 	= Male | Female

derive class iTask Person, Gender


myTask :: Task Int
myTask = enterInformation "Enter an integer" []

myTask2 :: Task Person
myTask2 = enterInformation "Enter data" []

myTask3 :: Task [Person]
myTask3 = enterInformation "Enter a list of data" []


palindrome :: Task (Maybe String)
palindrome 
	=   		enterInformation "Enter a palindrome" []
    	>>* 	[ OnAction  ActionOk     (ifValue isPalindrome  (\v -> return (Just v)))
               	, OnAction  ActionCancel (always	            (return Nothing))
               	]
where
	isPalindrome s = l == reverse l where l = [e \\ e <-: s]





person1by1 :: [Person] -> Task [Person]
person1by1 persons
	=		enterInformation "Add a person" []
		    -|| 
		    viewInformation "List so far.." [] persons
     >>*  	[ OnAction (Action "Add" [])    (hasValue  (\v -> person1by1 [v : persons]))  
            , OnAction (Action "Finish" []) (always (return persons))
            , OnAction ActionCancel 	    (always (return []))
	        ]


calculateSum :: Task Int
calculateSum
  =   			enterInformation ("Number 1","Enter a number") []
  	>>= \num1 ->	enterInformation ("Number 2","Enter another number") []
 	>>= \num2 ->	viewInformation   ("Sum","The sum of those numbers is:") [] (num1 + num2)
 	


twoTasks :: w1 w2  -> Task a      	   | iTask a & toUserConstraint w1 & toUserConstraint w2 
twoTasks user1 user2   
= withShared defaultValue (\share -> (user1  @: updateSharedInformation  ("Shared with" <+++ toTitle user2) [] share)
		              		                 -||  
		              		         (user2  @: viewSharedInformation   ("Shared with" <+++ toTitle user1)  [] share) )



twoTasksTest :: Task [Person]
twoTasksTest = twoTasks (UserWithId "Alice") (UserWithId "Bob")


//chat :: Task Void
//chat = 					get currentUser
        //>>= \me -> 		enterChoiceWithShared "Select someone to chat with:" [] users
        //>>= \you -> 	withShared ("","") (duoChat you me)

//duoChat ::  User User  (Shared (String, String)) -> Task Void
//duoChat  you me sharedNotes
 //=	                   chatWith you toView fromView sharedNotes
    // -||- 
                //(you @: chatWith me (toView o switch) (switch o fromView) sharedNotes)
//where
     //toView 	  (you, me) 				= (Display you, Note me)
     //fromView  (Display you, Note me) 	= (you, me) 
    //switch 	  (you, me) 				= (me, you)

chatWith :: User ((String, String) -> (Display String, Note))
            ((Display String, Note) -> (String, String))
            (Shared (String, String)) -> Task Void
chatWith who toV fromV notes
= 		updateSharedInformation ("Chat with " <+++ who) [UpdateWith toV (\_ view -> fromV view)] notes @! Void
	//>>*	[OnAction (Action "Stop" []) (always (return Void))]


//-----


:: Bicycle = { id :: Int }

derive class iTask Bicycle


station :: Shared [Bicycle]
station = sharedStore "Bikes" []

bikes 
	=				set [{id = i} \\ i <- [0..9]] station
	>>| 			updateSharedInformation "update bikes in station" [] station
	>>|				get station
	>>= \bikes ->	viewInformation "number of bikes in station:" [] (length bikes)
	

bikes2
	=				(		set [{id = i} \\ i <- [0..3]] station
					>>|		updateSharedInformation "update bikes in station" [] station)
					-&&-
					(wait "lets wait for enough bikes" (\bikes -> length bikes >= 6) station
					>>= \bikes -> viewInformation "we have enough bikes" [] bikes)	
					
	
	






