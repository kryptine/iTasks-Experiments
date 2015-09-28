module examples
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin

// You need to have or to make an "example.prj" file (
// If it is not avaialble, open and select examples.icl, the choose "File/New Project ..." in the Clean IDE
// Choose "Environment/iTaskTonic" environment as environment.
// Choose "Project/Update and Run (Ctrl R)" to compile and execute this module
// Repair the errors reported by the Clean compiler...

// To work with the iTask tasks defined in this module, start your browser and surf to "localhost".
// You can login as "root", "alice", "bob", "carol", "dave", "eve", "fred"... with as password the same as the login name.
// You can login with any number of users.
// If you like, you can add other users with the task "Manage users".
//
// When you want to use Tonic to inspect the work that is going on, surf to "localhost/tonic" 
//
// If you know the IP address of the machine this application is running on, you can login with any other browser running on any other machine.
// It should work on any browser supporting Html 5, but we strongly recommend Google Chrome.
//
// Known issues:
// You cannot create an new aplication when the old one is still running, it results in a linker error.
// It may happen that you start the application, while the previous version is still running. You have to kill the previous one first.
// If the browser does not show anything: remove the "examples-data" and "tonic" folders from directory where this examples.icl file is located, and restart the application.
// If a browser page only shows fragments: use an html 5 compatible browser.
// If you have started a task, but you don't see it appear: click somewhere in the page, and the new task should appear now.
// If you inspect a task value, but it is not shown: wait until a new value has been created with the task or refresh the tab (close it and open it again)
// It may take a while when Google maps are syncronized between the users, and due to a known bug, sometimes updates get lost... 


Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "CEFP 2015 iTasks & Tonic examples" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
        ] world
 

 
myExamples :: [Workflow]
myExamples = 	[	workflow "enter a value" 				"Enter a value and inspect its result"  enterValue
				,	workflow "palindrome" 					"accepts palindrome string" 	palindrome
				,	workflow "create list of persons"		"one by one" 					person1by1
				,	workflow "delegate  list of persons"	"delegate the creation" 		(delegate person1by1)
				,	workflow "monitor a list maker"			"one user creates a list and another can monitor this" monitorPersons

				,	workflow "monitor a GoogleMap browser"	"one user browses and another can monitor this" monitorGoogleMap
				,	workflow "chat 1"						"two users can chat with each other" chat1
				,	workflow "chat 2"						"two users can chat with each other, and point to interesting points on a map" chat2


				,	workflow "Manage users"					"Create users and logins..." 		manageUsers
			 	]


// Exercise 1: compile and run the application
// Surf to localhost, login, and start the task below. 
// Surf to localhost/tonic, inspect the static blueprints and see if you understand what the tasks are doing.
// Select the dynamic blueprints and inspect the running tasks values... 

// -- enter an integer number

enterValue :: Task Int
enterValue = enterInformation "Enter a value" [] >>= \number -> viewInformation "You entered the value: " [] number

// Exercise 2: Change the type of enterValue to :: Task [Person] and try again
// Define your own type (algebraic data type / record type) :: MyType = .... | ...| ... or :: MyType = { .. :: .., .. :: .. } and try again.
// Don't forget to add: derive class iTask MyType

// -- palindrome
 
palindrome :: Task (Maybe String)
palindrome 
	=   		enterInformation "Enter a palindrome" []
    	>>* 	[ OnAction  ActionOk     (ifValue isPalindrome  (\v -> return (Just v)))
               	, OnAction  ActionCancel (always	            (return Nothing))
               	]
where
	isPalindrome s = l == reverse l where l = [e \\ e <-: s]
	
// Exercise 3: Define a task where you can type in a list of integer numbers and add actions to eiter add, subtract or multiply these numbers.

// -- person 1 by 1

:: Person 	=  { name :: String, gender :: Gender, dateOfBirth :: Date}
:: Gender 	= Male | Female

derive class iTask Person, Gender

person1by1 :: Task [Person] 
person1by1 = add1by1 []

add1by1 :: [a] -> Task [a]	| iTask a
add1by1 list_so_far
	= 		enterInformation "Add an element" []
		    -|| 
		    viewInformation "List so far.." [] list_so_far
		>>* [ OnAction  (Action "Add" [])      (hasValue  (\elem   -> add1by1 [elem : list_so_far])) 
		    ,  OnAction  (Action "Finish" [])  (always (return list_so_far))
		    ,  OnAction  ActionCancel 	      (always (return []))
	        ]

// Exercise 4: Define a task integers1by1 which creates a list of Int in the same style as persons1by1.

// Exercise 5: See exercise 4, but now do it again for the type MyType you have defined in Exercise 2.

// -- delegate

delegate :: (Task a) -> Task a | iTask a
delegate task
	=					enterChoiceWithShared "Select someone to delegate the task to:" [] users
		>>= \user -> 	(user,"delegated task") @: (task >>= return)
		>>= \result ->	viewInformation "The result is:" [] result


// Exercise 6: Use delegate to let someone else create a list of values of the type MyType defined in Exercise 2.

// -- Monitor the work of someone else

monitorPersons ::  Task [Person]
monitorPersons =	selectCoWorker "who do you want to monitor ? " >>= monitorWorker 


import iTasks.API.Extensions.GIS.GoogleMap

monitorGoogleMap ::  Task (GoogleMap, Note)
monitorGoogleMap =	selectCoWorker "who do you want to monitor ?" >>= monitorWorker 

selectCoWorker :: String -> Task (User,User)
selectCoWorker  prompt
	=						get currentUser
		>>= \me -> 			enterChoiceWithShared prompt [] users
		>>= \colleague ->	return (me,colleague)


monitorWorker :: (User, User) -> Task a  | iTask a
monitorWorker (me,worker)
	=	withShared defaultValue 
		(\share -> ((worker, "Update Information") @: updateSharedInformation  ("Update, viewer is " <+++ me) [] share)
			        -||  
			       ((me, "View Information") @: viewSharedInformation    ("Viewer, worker is " <+++ worker)  [] share) 
	    )

// Exercise 7: Let someone else work on creating a value of you favorite type MyType and monitor its progress while the work takes place

// -- Chat variants

chat1 :: Task (Note,Note)
chat1 = chat
	    
chat2 :: Task ((GoogleMap,Situation),Reaction)
chat2 = chat

:: Situation	= AllIsFine | Fire | WaterProblem | TrafficAccident | OtherIssue Note
:: Reaction		= NoActionProposed | SendFireBrigate | SendPolice | SendSpecialForces | OtherProposal Note

derive class iTask Situation, Reaction

// chat: one worker can provide information of arbitrary type a which can be inspected by someone else
// while the other can show its work of arbitrary type b

chat :: Task (a,b) | iTask a & iTask b
chat =							selectCoWorker "with whom do you want to work ? "
		>>= \(me,colleague) ->	withShared defaultValue 
			(\workOfMe ->		withShared defaultValue
			(\workOfColleague ->((me,"chat") @: updateAndView (me,workOfMe) (colleague,workOfColleague))
								-&&-
								((colleague,"chat") @: updateAndView (colleague,workOfColleague) (me,workOfMe))
			))

updateAndView :: (User,Shared a) (User,Shared b) -> Task a | iTask a & iTask b
updateAndView (me,workOfMe) (you,workOfYou)
	= 	viewSharedInformation    ("You are looking at the response of: " <+++ you) [] workOfYou 
		||-
		updateSharedInformation  ("Please enter your information: " <+++ me) [] workOfMe
			
	    
// Exercise 7: play wit chat1 and chat2. Make a useful variant chat3 with your own favority types.

	    
	    



