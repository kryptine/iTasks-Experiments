module TryiTasks
import iTasks, Text, Text.HTML
import iTasks.API.Extensions.Editors.Ace
from StdFunc import o
/**
* This application let's you play with small iTask programs. It a sort of very minimal 'IDE'.
* You can choose example programs, modify them and build/run them
*/

//Code examples: Should be read from 
examples :==
	[("Hello World",join "\n"
		["main :: Task String"
		,"main = viewInformation () [] \"Hello World\""
		])
	,("Sum",join "\n"
		["main :: Task Int"
		,"main = enterInformation \"Enter a number\" [] >>= \\num1 ->" 
		,"       enterInformation \"Enter a second number\" [] >>= \\num2 ->" 
		,"       viewInformation \"The sum is: \" [] (num1 + num2)"
		])
	]

startCode :== snd (hd (examples)) //Code of the first example

//Paths to the required Clean and iTasks tools for building
cpmBin :== "/Users/bas/Clean/bin/cpm"

tryiTasks :: Task ()
tryiTasks = 
	(withShared startCode
		\scode ->			
			(loadExample scode -&&- modifyCode scode) @ snd
	) >^* [OnAction (Action "Build and Run") (hasValue buildAndRun)]
	@! () 

loadExample :: (Shared String) -> Task ()
loadExample scode = (forever (
		enterChoice "Load example" [ChooseFromDropdown fst] examples @ snd
		>>* [OnAction (Action "Load" ) (hasValue (\example -> set example scode))]
	) @! ())

//loadExample scode = viewInformation "Load example" [] "Here should be the selection of examples but it messes up the buttons somehow :(" @! ()

modifyCode :: (Shared String) -> Task String
modifyCode scode = updateSharedInformation "Edit code" [UpdateUsing id (const id) aceTextArea] scode

buildAndRun :: String -> Task ()
buildAndRun fragment = withTemporaryDirectory 
	\buildDir ->
			createCleanModule fragment buildDir
		>>| createCleanProject buildDir
		>>| buildCleanProject buildDir
		>>| runExecutable buildDir
		>>* [OnAction ActionClose (always (return ()))]
where
	//Create a clean module from the fragment
	createCleanModule fragment buildDir = exportTextFile (buildDir </> "test.icl") content
	where
		content = join "\n" 
			["module test"
			,"import iTasks"
			,fragment
			,"Start w = startEngine main w"
			]
		
	//Create a Clean project build the module"
	createCleanProject buildDir
		= 	callProcess "Create project" [] cpmBin ["project","test","create"] (Just buildDir)
		>>| setProjectOptions buildDir //Should be possible to do with command line options in cpm
	where
		setProjectOptions buildDir
			=  				importTextFile projectFile
			>>- \content -> exportTextFile projectFile (setOptions content)
		where
			projectFile = buildDir </> "test.prj"
			setOptions s = ( replaceSubString "Target:\tStdEnv" "Target:\tiTasks"
						   o replaceSubString "HeapSize:\t2097152" "HeapSize:\t20971520") s

	//Build the Clean project will be build to create the executable
	buildCleanProject buildDir
		= callProcess "Building project" [] cpmBin ["test.prj"] (Just buildDir)

	//"Here the generated executable will be run to test"
	runExecutable buildDir
		= 	callProcess "Your code is running" [ViewAs toView] (buildDir </> "test.exe") ["-port","8088"] (Just buildDir)
	where
		toView _ = ATag [HrefAttr "http://localhost:8088/",TargetAttr "_blank"] [Text "View the code at: http://localhost:8088"]

Start w = startEngine tryiTasks w

