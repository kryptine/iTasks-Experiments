implementation module MultiUser

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin

StartMultiUserTasks :: [Workflow] *World -> *World
StartMultiUserTasks workflows world 
	= startTask [ workflow "Manage users"  "Manage system users..."   manageUsers
				: workflows
				] world

startTask taskList world
	= startEngine [ publish "/" (WebApp []) (\_-> browseExamples taskList)
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
	








	
	
