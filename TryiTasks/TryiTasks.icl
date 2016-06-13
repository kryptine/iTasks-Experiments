module TryiTasks
import iTasks
/**
* This application let's you play with small iTask programs. It a sort of very minimal 'IDE'.
* You can choose example programs, modify them and build/run them
*/

startCode :== "start :: Task String\nstart = updateInformation () [] \"Hello World\"\n"

tryiTasks :: Task ()
tryiTasks = 
	(withShared startCode
		\scode ->			
			(loadExample scode -&&- modifyCode scode) @ snd
	) >^* [OnAction (Action "Build and Run" []) (hasValue buildAndRun)]
	@! ()

loadExample :: (Shared String) -> Task ()
loadExample scode = viewInformation "Load example" [] "Here you will be able to select an example code fragment and load it to the shared state" @! ()

modifyCode :: (Shared String) -> Task String
modifyCode scode = updateSharedInformation "Edit code" [UpdateWith toView fromView] scode
where
	toView s = Note s
	fromView _ (Note s) = s

buildAndRun :: String -> Task ()
buildAndRun fragment = withTemporaryDirectory 
	\buildDir ->
			createCleanModule fragment buildDir
		>>| createCleanProject buildDir
		>>| buildCleanProject buildDir
		>>| runExecutable buildDir
		
	>>| return ()
where
	createCleanModule fragment buildDir = viewInformation "Create module" [] "Here a clean module will be created from the fragment"
	createCleanProject buildDir = viewInformation "Create project" [] "Here a Clean project will be created to build the module"
	buildCleanProject buildDir = viewInformation "Build project" [] "Here the Clean project will be build to create the executable"
	runExecutable buildDir = viewInformation "Run executable" [] "Here the generated executable will be run to test"

Start w = startEngine tryiTasks w

