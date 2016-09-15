module Channel

import iTasks.Framework.Client.RunOnClient
import iTasks.Framework.SDSService
import Text
import StdMisc

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow ("Editors on shared note") 		"edit notes" 				notes,
	 workflow ("Exposed parametric view test") 	"test" 						testExposed,
	 workflow ("Server") 						"server" 					server,
	 workflow ("Client") 						"client" 					client
	]

notes :: Task String
notes 
	= withShared "" (\note -> exposeShared note body)
where
	body _ note 
		= runOnClient (
		 	viewSharedInformation "view on note" [] note
			-||-
			updateSharedInformation "edit shared note 1" [] note
			-||-
			updateSharedInformation "edit shared note 2" [] note
		)

:: ABC = A Int | B String | C Char

instance toString ABC
where
	toString (A i) = "ABC" +++ toString i
	toString (B s) = "ABC" +++ s	
	toString (C c) = "ABC" +++ toString c

derive class iTask ABC

testSDS :: RWShared ABC String String
testSDS = createReadWriteSDS ns "test1" get put
where
  ns = "test"

  get p iworld 
  	= case loadValue ns (toString p) iworld of
  		(Just a, iworld)  =	(Ok a, iworld)
  		(Nothing, iworld) = (Ok "", iworld) 
  
  put p w iworld = (Ok (inv p), storeValue ns (toString p) w iworld)
  inv p1 p2 = toString p1 == toString p2	

testExposed :: Task String
testExposed = exposeShared testSDS body
where
  body _ sds = runOnClient (
  				 enterInformation "Focus" [] >>= \p -> updateSharedInformation "Value" [] (sdsFocus p sds) 
  			   )

server :: Task String
server = exposeShared testSDS body
where
  body url sds = (viewSharedInformation "Original" [] (sdsFocus (A 1) sds)) ||- 
  				 (viewInformation "URL" [] url ||- (updateSharedInformation "Value" [] (sdsFocus (A 1) sds) ||- updateSharedInformation "Value" [] (sdsFocus (A 1) sds)))

client :: Task String
client = enterInformation "URL" [] >>= \url -> openRemoteSDS url body
where
	body :: (Maybe (RWShared ABC String String)) -> Task String
	body Nothing  = viewInformation "Error" [] "Failed to open remote SDS"
	body (Just s) = enterInformation "Value" [] >>= \v -> set v (sdsFocus (A 1) s) 

Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist basicAPIExamples)) world


