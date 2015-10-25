implementation module MultiUser

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin

StartMultiUserTasks :: [Workflow] [PublishedTask] *World -> *World
StartMultiUserTasks workflows tasks world 
	= startTask [ workflow "Manage users"  "Manage system users..."   manageUsers
				: workflows
				] tasks world

startTask taskList tasks world
	= startEngine [ publish "/" (WebApp []) (\_-> browseExamples taskList)
                  : tasks
				  ] world
where
	browseExamples taskList = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (browseAuthenticated taskList))
			] )
	
	browseAuthenticated taskList {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist taskList)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	








	
	
