module Highlighter

import iTasks, Tasklet, Text
import syncol, EdTab

//-------------------------------------------------------------------------

highlighterTasklet :: Tasklet Void Void 
highlighterTasklet = 
	{ Tasklet
	| generatorFunc		= highlighterGUI
	, resultFunc		= \_ = Value Void False
	, tweakUI  			= id
	}

highlighterGUI :: !TaskId (Maybe Void) !*IWorld -> *(!TaskletGUI Void, !Void, !*IWorld)

highlighterGUI _ _ iworld  

	# gui = { TaskletHTML
			| width  		= ExactSize 800
			, height 		= ExactSize 600
			, html   		= HtmlDef 
								("<textarea id=\"inp\" rows=\"15\" cols=\"120\"></textarea>" +++
								 "<br/>" +++
								 "<input id=\"start\" type=\"button\" value=\"Highlight!\">" +++
								 "<br/>" +++
								 "<div id=\"out\">")
			, eventHandlers = [HtmlEvent "start" "click" onClick]
			}
			
	= (TaskletHTML gui, Void, iworld)
where		
	onClick _ _ _ d
		# (d, str) = getDomAttr d "inp" "value"
		# lines = split "\n" str
		# parsedLines = firstParse (toStrictList lines)
		# hls = genText parsedLines
		# text = join "<br/>" hls
		# (d, _) = setDomAttr d "out" "innerHTML" text
		= (d, Void)

//-------------------------------------------------------------------------

source = 
	["// type",
	 ":: T = T",
	 "",
	 "tasklet1 :: Task Int",
	 "tasklet1",
	 "	= 		mkTask pushTasklet",
	 "		>>* [ OnAction ActionOk (ifValue (\n -> n >= 3)) returnV",
	 "            ]"]

instance toString Info
where
	toString info
		= "comment: " +++ toString info.comment_level +++ ", " +++
		  "typedef: " +++ toString info.is_typedef +++ ", " +++
		  "typedec: " +++ toString info.is_typedecl +++ ", " +++
		  "level: " +++ toString info.offside_level +++ ", " +++
		  "flush: " +++ toString info.flush  

toStrictList [l:ls] = SCons l (toStrictList ls)
toStrictList [] = SNil

fromStrictList (SCons l ls) = [l:fromStrictList ls]
fromStrictList SNil = [] 

genText :: (StrictList (!Info,!String)) -> [String]
genText (SCons l ls) 
	# pic = tabDrawStringC l DefaultSyntaxColours newPicture
	= [fst (asString pic) : genText ls]
	
genText SNil = []

/*
Start
	# lines = toStrictList source 
	# parsedLines = firstParse lines
	# pic = genText parsedLines newPicture
	= fst (asString pic)
*/
/*
Start :: *World -> *World
Start world = startEngine 
				[{PublishedTask|url="/bubu"
				               ,task=TaskWrapper (\_ -> mkTask highlighterTasklet) 
				               ,defaultFormat = JSONGui}] 
				world
*/
Start world = startEngine (mkTask highlighterTasklet) world

 
 
 
 