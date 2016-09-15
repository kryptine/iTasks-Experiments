module Editlet

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.CodeMirror
import iTasks.API.Extensions.GIS.GoogleMap
import iTasks.API.Extensions.GIS.Leaflet

import StdArray

import StdDebug

:: TimeDelta = SetSec !Int | SetMin !Int | SetHour !Int
derive class iTask TimeDelta

/*
buienLet :: Editlet String Void
buienLet = toEditlet simpl
where
	simpl =	EditletSimpl "Buienradar" {EditletSimplDef
				| genUI 	= \_ world = (uiDef, world)
				, updateUI 	= \_ _ a w = (a,w)
				, genDiff 	= \_ _ -> Nothing
				, appDiff 	= \_ v -> v
				}

	uiDef = { html 			= RawText ("<a href=\"http://www.buienradar.nl\" target=\"_blank\"><img border=\"0\" src=\"http://m.buienradar.nl/\"></a>")
			, eventHandlers = []
			, width 		= ExactSize 300
			, height 		= ExactSize 300
			}

:: StringDelta = {newString :: String}
derive class iTask StringDelta

stringlet :: Editlet String [String]
stringlet = toEditlet simpl
where
	simpl = EditletSimpl "Hello world" {EditletSimplDef
		    	| genUI		= \cid world -> (uiDef cid, world)
				, updateUI 	= onUpdate
				, genDiff  	= \o n -> if (o == n) Nothing (Just [n,n])
				, appDiff  	= \n _ -> hd n
				}

	uiDef cid 
		  = { html 			= TextareaTag [IdAttr cid] []
		  	, eventHandlers = [ComponentEvent cid "keyup" onChange]
		  	, width 		= ExactSize 640
		  	, height 		= ExactSize 480
		  	}

	onUpdate :: ComponentId (Maybe [String]) String *JSWorld -> (!String, !*JSWorld)
	onUpdate id _ val world
		# world	= (getElementById id .# "value" .= val) world
		= (val,world)
	
	onChange  :: ComponentId {JSObj JSEvent} String *JSWorld -> (!String, !*JSWorld)
	onChange id _ val world
		= let (val, w) = .? (getElementById id .# "value") world in (jsValToString val, w)
		
timelet :: Time -> Editlet Time [TimeDelta]
timelet t =	toEditlet simpl
where
	simpl = EditletSimpl t {EditletSimplDef
				| genUI		= \cid world -> (uiDef cid, world)
				, updateUI	= onUpdate				
				, genDiff	= genDiff
				, appDiff	= appDiff
				}

	uiDef cid 
		  = { html 			= RawText ("<div style=\"font-size: 24pt;\" id=\"" +++ cid +++ "\"></div>")
		  	, eventHandlers = []
		  	, width 		= ExactSize 320
		  	, height 		= ExactSize 240
		  	}

	onUpdate ::  ComponentId (Maybe [TimeDelta]) Time *JSWorld -> (!Time,!*JSWorld)
	onUpdate id _ val world
		# world = (getElementById id .# "innerHTML" .= (toString val)) world
		# world	= (getElementById id .# "style" .# "color" .= (colors !! (val.Time.sec rem (length colors)))) world
		= (val,world)
		
	colors = ["#f0f","#00f","#f00","#30f","#ff0","#66f"]
	
	genDiff :: Time Time -> Maybe [TimeDelta]
	genDiff t1 t2 = case (  (if (t1.Time.sec == t2.Time.sec) [] [SetSec t2.Time.sec])
						 ++ (if (t1.Time.min == t2.Time.min) [] [SetMin t2.Time.min])
						 ++ (if (t1.Time.hour == t2.Time.hour) [] [SetHour t2.Time.hour])
						 ) of [] = Nothing ; delta = Just delta

	appDiff :: [TimeDelta] Time -> Time
	appDiff [] t = t
	appDiff [SetSec s:d] t = appDiff d {Time|t & sec = s}
	appDiff [SetMin m:d] t = appDiff d {Time|t & min = m}
	appDiff [SetHour h:d] t = appDiff d {Time|t & hour = h}	


clocklet :: Time -> Editlet Time Time
clocklet t = toEditlet simpl
where
	simpl = EditletSimpl t {EditletSimplDef 
				| genUI		= \cid world -> (uiDef cid, world)
				, updateUI	= onInit					
				, genDiff	= \t1 t2 -> if (t1 == t2) Nothing (Just t2)
				, appDiff	= \tn to -> tn
				}

	uiDef cid 
		  = { html 			= RawText ("<canvas height=\"100%\" id=\"" +++ cid +++ "\" class=\"CoolClock\"></canvas>")
		  	, eventHandlers = []
		  	, width 		= ExactSize 320
		  	, height 		= ExactSize 240
		  	}

	onInit :: ComponentId (Maybe Time) Time *JSWorld -> (!Time, !*JSWorld)
	onInit id Nothing val world
		# world				= addJSFromUrl "/coolclock.js" Nothing world
		# world				= addJSFromUrl "/moreskins.js" Nothing world
		= (val,world)

	// Update
	onInit id mbDiff val world = (val, world)
	
	onLoad :: *JSWorld -> *JSWorld
	onLoad world
		# (window,world)	= jsWindow world
		# (coolclock,world)	= jsGetObjectAttr "CoolClock" window world
		//# (coolclock,world)	= findObject "CoolClock" world
		# (err,world)		= jsIsUndefined coolclock world
		| err = trace_n "No CoolClock" world
		# (method,world)	= jsGetObjectAttr "findAndCreateClocks" coolclock world
		# (err,world)		= jsIsUndefined method world
		| err = trace_n "No findAndCreateClocks" world
		# (_,world)			= callObjectMethod "findAndCreateClocks" [] coolclock world
		= world
/*		
	onUpdate :: ComponentId (JSPtr JSObject) Time *JSWorld -> (!Time, !*JSWorld)
	onUpdate id event val=:{Time|hour,min,sec} world
		# (coolclock,world)	= findObject "CoolClock" world
		# (err,world)		= jsIsUndefined coolclock world
		# (method,world)	= jsGetObjectAttr "findAndCreateClocks" coolclock world
		# (err,world)		= jsIsUndefined method world
		# (_,world)			= jsCallObjectMethod "findAndCreateClocks" [] coolclock world			
		# (config,world)	= jsGetObjectAttr "config" coolclock world
		# (err,world)		= jsIsUndefined config world		
		# (tracker,world)	= jsGetObjectAttr "clockTracker" config world
		# (err,world)		= jsIsUndefined tracker world
		# (myclock,world)	= jsGetObjectAttr id tracker world
		# (err,world)		= jsIsUndefined myclock world	
		# (_,world)			= jsCallObjectMethod "setTime" [hour,min,sec] myclock world
		= (val,world)
*/

:: Game        = { board :: !TicTacToe     // the current board
                 , names :: !Players       // the current two players
                 , turn  :: !TicTac        // the player at turn
                 }
:: Players     = { tic   :: !Name          // the tic player is starting
                 , tac   :: !Name          // the tac player
                 }
:: Name      :== String
:: TicTacToe :== [[Tile]]
:: Tile        = Clear | Filled TicTac
:: TicTac      = Tic | Tac
:: Coordinate  = {col :: Int, row :: Int}  // 0 <= col <= 2 && 0 <= row <= 2

instance ~   TicTac     where ~  Tic     = Tac
                              ~  Tac     = Tic
derive class iTask Tile, TicTac, Coordinate

tictactoelet :: (TicTacToe,TicTac) -> Editlet (TicTacToe,TicTac) (TicTacToe,TicTac)
tictactoelet t=:(board,turn) = toEditlet simpl
where
	simpl = EditletSimpl t {EditletSimplDef
				| genUI		= \cid world -> (uiDef cid, world)
				, updateUI  = onUpdate
				, genDiff	= \t1 t2 -> if (t1 === t2) Nothing (Just t2)
				, appDiff	= \tn to -> tn
				}
	uiDef cid
		  = { html 			= DivTag [IdAttr "tictactoe"] [init_board "tictactoe" t]
		  	, eventHandlers = [ComponentEvent (cellId "tictactoe" c) "click" (onCellClick c) \\ c <- [{col=x,row=y} \\ x <- [0..2] & y <- [0..2] ]]
		  	, width 		= ExactSize 640
		  	, height 		= ExactSize 480
		  	}

	//onInit :: ComponentId (JSPtr JSObject) (TicTacToe,TicTac) *JSWorld -> (!(TicTacToe,TicTac), !*JSWorld)
	//onInit editorId _ state world = (state,redraw "tictactoe" state world)

	onUpdate :: ComponentId (Maybe (TicTacToe,TicTac)) (TicTacToe,TicTac) *JSWorld -> (!(TicTacToe,TicTac), !*JSWorld)
	onUpdate editorId _ state world = (state,world) //(state,redraw "tictactoe" state world)

	onCellClick :: Coordinate ComponentId {JSObj JSEvent} (TicTacToe,TicTac) *JSWorld -> (!(TicTacToe,TicTac), !*JSWorld)
	onCellClick coord editorId event (board,turn) world
		# state = (add_cell coord turn board, ~turn)
		= (state, redraw "tictactoe" state world)
		
	redraw	:: !String !(TicTacToe,TicTac) *JSWorld -> *JSWorld
	redraw editorId state world = (getElementById editorId .# "innerHTML" .= (toString (init_board editorId state))) world
	
	init_board :: !String !(TicTacToe,TicTac) -> HtmlTag
	init_board editorId (board,turn)
		= TableTag [BorderAttr "0"] 
			           [ TrTag [] [  cell {col=x,row=y} \\ x <- [0..2] ] \\ y <- [0..2] ]
	where
		cell c	= case lookup1 c (tiles board) of
				    Filled t = TdTag [] [TileTag (64,64) t]
				    Clear    = TdTag [AlignAttr "center"] [ButtonTag [IdAttr (cellId editorId c)] [Text "Choose"]]

		TileTag (w,h) t	= ImgTag [ SrcAttr ("/" <+++ t <+++ ".png"), WidthAttr (toString w), HeightAttr (toString h) ]

	cellId editorId {col,row} = editorId <+++ "-" <+++ col <+++ row

	tiles :: !TicTacToe -> [(Coordinate,Tile)]
	tiles board
		= flatten [ [ ({col=x,row=y},cell)
		            \\ cell <- row & x <- [0..]
		            ]
		          \\ row <- board & y <- [0..]
		          ]
	add_cell :: !Coordinate !TicTac !TicTacToe -> TicTacToe
	add_cell new turn board
		= [ [  if (new === {col=x,row=y}) (Filled turn) cell
		    \\ cell <- row & x <- [0..]
		    ]
		  \\ row <- board & y <- [0..]
		  ]
		  
	lookup1 key table = hd [v \\ (k,v) <- table | k === key]


empty_board :: TicTacToe
empty_board = repeatn 3 (repeatn 3 Clear)
*/

defcm = { configuration = [CMMode "haskell", CMLineNumbers True]
        , position = (0,0)
        , selection = Nothing
        , highlighted = []
        , source = ["Buu1","Buu2"]}

//test5 = updateInformation "CodeMirror" [] (codeMirrorEditlet "buu")

test5 :: Task CodeMirror
test5 = withShared defcm (\defcm -> updateSharedInformation "CodeMirror Settings" [] defcm
                                                                -|| 
                                   updateSharedInformation "CodeMirror Editor" 
                                                   [UpdateWith (\cm -> codeMirrorEditlet cm [])
                                                               (\_ editlet -> editlet.currVal)] defcm )


        
//test5 = updateInformation "CodeMirror" [] (codeMirrorEditlet gDefault{|*|} [])

/*
test4 = updateInformation "Tic tac toe" [] (tictactoelet (empty_board,Tic))
	
test2 = updateInformation "Test" [] (timelet (fromString "13:00:00"))

test3 = viewSharedInformation "Clock2" [] (mapRead (\t -> (timelet t,clocklet t)) currentTime)
		//||- viewInformation (Title "Buienradar") [] buienLet
		//<<@ AfterLayout (uiDefSetDirection Horizontal)
		
//test = viewSharedInformation "Clock" [ViewWith timeEditlet] currentTime

test = updateInformation "String" [] stringlet @ (\editlet -> editlet.currVal) >&> viewSharedInformation "DEBUG" []

//test6 = viewInformation "JointJS" [] (jointJSEditlet JointJS)

//test7 :: Task LeafletMap
//test7 = enterInformation "Test" [] 
*/

Start :: *World -> *World
Start world = startEngine test5 world


    
