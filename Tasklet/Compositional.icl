module Compositional

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface

import Data.Maybe, Text

import StdArray, StdMisc, StdDebug

/*
withEditlet editlet dp (val,mask,ver) meta vst
	= gEditor{|*|} dp ({editlet & currVal = val},mask,ver) meta vst

gEditor{|Drawing|} dp vv meta env = withEditlet painterEditlet dp vv meta env
//gEditor{|Drawing|} = withEditlet painterEditlet

withEditlet2 editlet dp upd (val,mask) env
    # ((editlet,mask),env) = gUpdate{|*|} dp upd ({editlet & currVal = val},mask) env
    = ((editlet.currVal,mask),env) 

gUpdate{|Drawing|} dp upd vv iworld = withEditlet2 painterEditlet dp upd vv iworld
//gUpdate{|Drawing|} = withEditlet2 painterEditlet
*/
/*
gEditor{|(,)|} dp (val,mask,ver) meta env 
	= gEditor{|*|} dp (toEditlet val,mask,ver) meta env

gUpdate{|(,)|} dp upd (val,mask) env
    # ((editlet,mask),env) = gUpdate{|*|} dp upd (toEditlet val,mask) env
    = ((editlet.currVal,mask),env) 
*/

updateInformationEditlet :: v -> Task v | ToEditlet v a b & iTask v & iTask a & iTask b
updateInformationEditlet v = updateInformation "title" [UpdateWith (\v -> toEditlet v) (\_ editlet -> editlet.currVal)] v

class ToEditlet sv ~ d ~ cl | iTask sv
where
	toEditlet :: sv -> Editlet sv d cl

instance ToEditlet Int Int ()
where
	toEditlet v 
	  = { Editlet
    	| currVal    = v
    
	    , defValSrv  = v
    
	    , genUI      = genUI
	    , initClient = initClient
	    , appDiffClt = appDiffClt
	    , genDiffSrv = genDiffSrv
	    , appDiffSrv = appDiffSrv
	    }
	where
		genUI cid world
			= ({ ComponentHTML
			   | html 			= InputTag [IdAttr cid, TypeAttr "text", ValueAttr (toString v)]
			   , width 			= FlexSize
		   	   , height			= FlexSize
		   	   }, world)

		onChange cid _ _ world
		    # (jsv, world) = .? ((getElementById cid) .# "value") world
			= ((), Diff (jsValToInt jsv) (\_ _ w -> ((),NoDiff,w)), world)

		initClient mkHandler cid world = ((), addEventListener onChange world)
		where
			addEventListener handler world
				# world = trace_n cid world
			 	= ((getElementById cid) .# "addEventListener" .$! ("change", mkHandler handler cid)) world
		
		genDiffSrv old new | old == new
			= Nothing
			= Just new

		appDiffSrv new _ = new

		appDiffClt mkHandler cid v () world
		 	= ((), ((getElementById cid) .# "value" .= (toString v)) world)
		

instance ToEditlet (a,b) (Maybe da, Maybe db) (ca,cb) | ToEditlet a da ca & ToEditlet b db cb
where
	toEditlet v = tupleEditlet v

tupleEditlet :: (a,b) -> Editlet (a,b) (Maybe da, Maybe db) (ca,cb) | ToEditlet a da ca & ToEditlet b db cb
tupleEditlet (a,b)
  = { Editlet
    | currVal    = (a,b)
    
    , defValSrv  = (aeditlet.Editlet.defValSrv, beditlet.Editlet.defValSrv)
    
    , genUI      = genUI
    , initClient = initClient
    , appDiffClt = appDiffClt
    , genDiffSrv = genDiffSrv
    , appDiffSrv = appDiffSrv
    }
    
where
	aeditlet = toEditlet a
	beditlet = toEditlet b

	cida cid = cid +++ "_1"
	cidb cid = cid +++ "_2"

	initClient mkHandler cid world 
		# (cla, world) = aeditlet.Editlet.initClient (liftHandler_a mkHandler) (cida cid) world
		# (clb, world) = beditlet.Editlet.initClient (liftHandler_b mkHandler) (cidb cid) world
		= ((cla, clb), world)
	
	liftHandler_a mkHandler fun cid 
		= mkHandler wrapper (subString 0 (textSize cid - 2) cid) // remove postfix
	where	
		wrapper cid eventObjs (cla,clb) world
			# (ncla, diff, world) = fun (cida cid) eventObjs cla world
			= ((ncla,clb), liftDiff diff, world)

		liftDiff NoDiff = NoDiff
		liftDiff (Diff d cont) = Diff (Just d, Nothing) (liftCont cont)

		liftCont cont conflict (cla, clb) world
			# (ncla, diff, world) = cont conflict cla world
			= ((ncla, clb), liftDiff diff, world)

	liftHandler_b mkHandler fun cid 
		= mkHandler wrapper (subString 0 (textSize cid - 2) cid) // remove postfix
	where	
		wrapper cid eventObjs (cla, clb) world
			# (nclb, diff, world) = fun (cidb cid) eventObjs clb world
			= ((cla,nclb), liftDiff diff, world)

		liftDiff NoDiff = NoDiff
		liftDiff (Diff d cont) = Diff (Nothing, Just d) (liftCont cont)

		liftCont cont conflict (cla, clb) world
			# (nclb, diff, world) = cont conflict clb world
			= ((cla, nclb), liftDiff diff, world)

	genDiffSrv (cla1,clb1) (cla2,clb2)
		= case (aeditlet.Editlet.genDiffSrv cla1 cla2, beditlet.Editlet.genDiffSrv clb1 clb2) of
			(Nothing, Nothing) = Nothing
			diff			   = Just diff

	appDiffSrv (da, db) (a, b)
		= (maybe a (\d -> aeditlet.Editlet.appDiffSrv d a) da,
		   maybe b (\d -> beditlet.Editlet.appDiffSrv d b) db)

	appDiffClt mkHandler cid (da, db) (cla, clb) world
		# (cla, world) = case da of
			(Just d) = aeditlet.Editlet.appDiffClt (liftHandler_a mkHandler) (cida cid) d cla world
					 = (cla, world)
					 
		# (clb, world) = case db of
			(Just d) = beditlet.Editlet.appDiffClt (liftHandler_b mkHandler) (cidb cid) d clb world
					 = (clb, world)

		= ((cla, clb), world)

	genUI cid world
		# (ahtml, world) = aeditlet.Editlet.genUI (cida cid) world
		# (bhtml, world) = beditlet.Editlet.genUI (cidb cid) world
		= ({ ComponentHTML
		   | html 			= DivTag [] [ahtml.ComponentHTML.html, bhtml.ComponentHTML.html]
		   , width 			= maxSize ahtml.ComponentHTML.width bhtml.ComponentHTML.width
		   , height			= maxSize ahtml.ComponentHTML.height bhtml.ComponentHTML.height
		   }, world)
		
	maxSize FlexSize _ = FlexSize   	
	maxSize _ FlexSize = FlexSize

	maxSize WrapSize _ = WrapSize   	
	maxSize _ WrapSize = WrapSize

	maxSize (ExactSize a) (ExactSize b) = ExactSize (max a b)   	

test = updateInformationEditlet (1,1)

Start :: *World -> *World
Start world = startEngine test world


    


