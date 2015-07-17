module Life

import iTasks
import MultiUser
import Graphics.Scalable
import iTasks.API.Extensions.SVG.SVGlet


Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow  "Life Game" "Life Game"  	 playGameOfLife
								  , workflow "omhoog" "omhoog" omhoog
								  ] world

derive class iTask LifeCell

// test to show behaviour of shares which are updated in the view

omhoog =  withShared 0 (updateSharedInformation "omhoog"  [UpdateWith id (\old _ -> old+1)])
         >>* [OnValue (ifValue (\v -> v > 100) return)
             ,OnAction ActionOk (hasValue (viewInformation "resultaat" []))
             ]

// Life Game using iTasks and SVG lib

playGameOfLife :: Task Generation
playGameOfLife 
	= withShared initLife play
where
	play generation
		=		updateSharedInformation "Enter Cells"     [imageUpdate id (mkBoard True)  (\old new -> new)]              generation
		>>| 	updateSharedInformation "Simulating Life" [imageUpdate id (mkBoard False) (\old _ -> fst (LifeGame old))] generation
		>>|		play generation

initLife = insertCell (newCell 1 1) (insertCell (newCell 1 2) (insertCell (newCell 1 3) []))

// drawing of a the cells ...

cellSize 	= 20.0
lengthX		= 10	
lengthY		= lengthX
boardSize 	= lengthX * lengthY

mkBoard active gen	
	= grid (Rows lengthY) (LeftToRight,TopToBottom) [] [] cells Nothing
where
	cells = [cell i j gen \\ i <- [0..lengthX-1], j <- [0..lengthY-1]]

	cell i j [] 	= showDeadCell i j
	cell i j [[{x,y}:ys]:xs]
	| x==i && y==j	= showLivingCell i j
	| x<=i && y<j	= cell i j [ys:xs]
	| x<=i       	= cell i j xs
	| otherwise		= showDeadCell i j
	cell i j [[]:xs] = cell i j xs
	  
	showLivingCell i j = whenInteractive (emptyCell <@< {fill = toSVGColor "red"})    {onclick = removeCell (newCell i j)} 
	showDeadCell   i j = whenInteractive (emptyCell <@< {fill = toSVGColor "black"})  {onclick = insertCell (newCell i j)} 

	whenInteractive image attr
	| active		= image <@< attr
	| otherwise		= image

	emptyCell 	= rect (px cellSize) (px cellSize) 
						<@< {stroke = toSVGColor "white"}


// Old LifeGame calculation

::	Generation	:==	[[LifeCell]]
::	LifeCell
	=	{	x	:: !Int
		,	y	:: !Int
		,	age	:: !Int
		}

newCell :: !Int !Int -> LifeCell
newCell x y
	= {x=x,y=y,age=0}


/*	Insert a LifeCell to a Generation. 
	In a Generation LifeCells are ordered by increasing x-coordinate first, and by increasing y-coordinate second.
*/
insertCell::!LifeCell !Generation -> Generation
insertCell c1=:{x=x1} gen=:[cs=:[{x=x2,y=y2}:x2ys] : cs_xs]
	| x2<x1			= [cs				: insertCell c1 cs_xs]
	| x2==x1		= [insertCelly c1 cs: cs_xs]
	| otherwise		= [[c1],cs			: cs_xs]
where
	insertCelly :: !LifeCell ![LifeCell] -> [LifeCell]
	insertCelly c1=:{y=y1} [c2=:{x=x2,y=y2}:x2ys]
		| y2<y1		= [c2	: insertCelly c1 x2ys]
		| y2==y1	= [c1	: x2ys]
		| otherwise	= [c1,c2: x2ys]
	insertCelly c1 _= [c1]
insertCell c1 []
	= [[c1]]

/*	Remove a LifeCell from a Generation.
*/
removeCell::!LifeCell !Generation -> Generation
removeCell c1=:{x=x1,y=y1} gen=:[cs=:[{x=x2,y=y2}:x2ys]:cs_xs]
	| x2<x1			= [cs:removeCell c1 cs_xs]
	| x2>x1			= gen
	# cs			= removeCelly c1 cs
	| isEmpty cs	= cs_xs
	| otherwise		= [cs : cs_xs]
where
	removeCelly :: !LifeCell ![LifeCell] -> [LifeCell]
	removeCelly c1=:{y=y1} cs=:[c2=:{x=x2,y=y2}:x2ys]
		| y2<y1		= [c2 : removeCelly c1 x2ys]
		| y2==y1	= x2ys
		| otherwise	= cs
	removeCelly _ _	= []
removeCell c [[]:cs_xs]
	= removeCell c cs_xs
removeCell c _
	= []

/*	Calculate the new Generation (first tuple result) and the Generation of LifeCells that die (second tuple result).
*/
LifeGame::!Generation -> (!Generation,!Generation)
LifeGame gen
	# (next,_,die)	= NextGen gen gen
	  next			= CelebrateSurvival next gen
	= (next,die)
where
	NextGen::!Generation Generation -> (!Generation,Generation,!Generation)
	NextGen [[c=:{x,y}:cs_x]:cs_xs] gen
		| Neighbours34 (Neighbours c gen)	= (insertCell c gennext1,new,diednext)
		| otherwise							= (gennext1,new,insertCell c diednext)
	where
		(gennext,newbornsnext,diednext)		= NextGen [cs_x:cs_xs] gen1
		(gennext1,new)						= NewBorns c newbornsnext gennext gen
		gen1								= ShiftGeneration [cs_x:cs_xs] gen
		
		Neighbours34 [_,_,_]	=  True
		Neighbours34 [_,_,_,_] 	=  True
		Neighbours34 _			=  False
		
		NewBorns::!LifeCell Generation Generation Generation -> (!Generation,Generation)
		NewBorns c newbornsnext gennext gen
			= NewBorns1 (NewBornNeighbours c gen) newbornsnext gennext gen
		where
			NewBorns1 [c=:{x=x1,y=y1}:cs] newbornsnext gennext gen
				| Neighbours3 (Neighbours c gen)	= (insertCell c gennext1,insertCell c newbornsnext1)
				| otherwise							= next_genANDnewborns
			where
				(gennext1,newbornsnext1)			= next_genANDnewborns
				next_genANDnewborns		 			= NewBorns1 cs newbornsnext gennext gen
				
				Neighbours3::![LifeCell] -> Bool
				Neighbours3 [_,_,_]	= True
				Neighbours3 _ 		= False	
			NewBorns1 [] newbornsnext gennext _
				= (gennext,newbornsnext)
			
			//	NewBornNeighbours c gen -> dead neighbours of c in gen in decreasing order.
			
			NewBornNeighbours::!LifeCell !Generation -> [LifeCell]
			NewBornNeighbours {x,y} gen
				= NewBornNeighbours1 (x-1) (x+1) (y-1) gen []
			where
				NewBornNeighbours1:: !Int !Int !Int !Generation ![LifeCell] -> [LifeCell]
				NewBornNeighbours1 x xn y [cs=:[{x=x2}:_]:cs_xs] newborns
					| x>xn		= newborns
					| x2<x		= NewBornNeighbours1 x xn y cs_xs newborns
					| x2==x		= NewBornNeighbours2 x y (y+2) cs (NewBornNeighbours1 (x+1) xn y cs_xs newborns)
					| otherwise	= [newCell x y,newCell x (y+1),newCell x (y+2):NewBornNeighbours1 (x+1) xn y cs_xs newborns]
				NewBornNeighbours1 x xn y [] newborns
					| x>xn		= newborns
					| otherwise	= [newCell x y,newCell x (y+1),newCell x (y+2):NewBornNeighbours1 (x+1) xn y [] newborns]
				
				NewBornNeighbours2:: !Int !Int !Int ![LifeCell] ![LifeCell] -> [LifeCell]
				NewBornNeighbours2 x y yn [c=:{x=x2,y=y2}:cs] cs_xs
					| y>yn		= cs_xs
					| y2<y		= NewBornNeighbours2 x y yn cs cs_xs
					| y2==y		= NewBornNeighbours2 x (y+1) yn cs cs_xs
					| otherwise	= [newCell x y:NewBornNeighbours2 x (y+1) yn cs cs_xs]
				NewBornNeighbours2 x y yn [] cs_xs
					| y>yn		= cs_xs
					| otherwise	= [newCell x y:NewBornNeighbours2 x (y+1) yn [] cs_xs]
		
		ShiftGeneration::!Generation !Generation -> Generation
		ShiftGeneration [[c=:{x,y}:_]:_] gen	= ShiftGeneration1 {c & x=x-2,y=y-2} gen
		ShiftGeneration [[],[c=:{x,y}:_]:_] gen	= ShiftGeneration1 {c & x=x-2,y=y-2} gen
		ShiftGeneration partial_gen gen			= gen
		
		ShiftGeneration1::!LifeCell !Generation -> Generation
		ShiftGeneration1 c=:{x=x1,y=y1} gen=:[[c2=:{x=x2,y=y2}:cs_x]:cs_xs]
			| x2<x1						= ShiftGeneration1 c cs_xs
			| x2==x1 && y2<y1			= ShiftGeneration1 c [cs_x:cs_xs]
			| otherwise					= gen
		ShiftGeneration1 c [[]:cs_xs]
			= ShiftGeneration1 c cs_xs
		ShiftGeneration1 c _
			= []
		
		//	Neighbours c gen -> neighbours of c in gen in decreasing order.
		
		Neighbours::!LifeCell !Generation -> [LifeCell]
		Neighbours {x,y} gen
			= Neighbours1 (x-1) (x+1) (y-1) gen []
		where
			Neighbours1:: !Int !Int !Int !Generation ![LifeCell] -> [LifeCell]
			Neighbours1 x xn y [cs=:[{x=x2,y=y2}:_]:cs_xs] neighbours
				| x2<x						= Neighbours1 x xn y cs_xs neighbours
				| x2<=xn					= Neighbours2 y (y+2) cs (Neighbours1 (x+1) xn y cs_xs neighbours)
				| otherwise					= neighbours
			Neighbours1 _ _ _ [] neighbours
				= neighbours
			
			Neighbours2:: !Int !Int ![LifeCell] ![LifeCell] -> [LifeCell]
			Neighbours2 y yn [c=:{x=x2,y=y2}:cs] cs_xs
				| y2<y						= Neighbours2 y yn cs cs_xs
				| y2<=yn					= [c:Neighbours2 (y+1) yn cs cs_xs]
				| otherwise					= cs_xs
			Neighbours2 _ _ [] cs_xs
				= cs_xs
	NextGen [[]:cs_xs] gen
		= NextGen cs_xs gen
	NextGen _ _
		= ([],[],[])
	
	CelebrateSurvival :: !Generation !Generation -> Generation
	CelebrateSurvival new old
		= map (map (celebrate old)) new
	where
		celebrate :: !Generation !LifeCell -> LifeCell
		celebrate old newcell
			| found		= {newcell & age=age+1}
			| otherwise	= {newcell & age=age}
		where
			(found,age)	= GetCellAge newcell old
		
		GetCellAge :: !LifeCell !Generation -> (!Bool,!Int)
		GetCellAge c1=:{x=x1} [xs=:[{x=x2}:_]:xss]
			| x1<x2		= (False,0)
			| x1>x2		= GetCellAge  c1 xss
			| otherwise	= GetCellAge` c1 xs
		GetCellAge _ _
			= (False,0)
		
		GetCellAge` :: !LifeCell ![LifeCell] -> (!Bool,!Int)
		GetCellAge` c1=:{y=y1} [{y=y2,age}:xs]
			| y1<y2		= (False,0)
			| y1>y2		= GetCellAge` c1 xs
			| otherwise	= (True,age)
		GetCellAge` _ _
			= (False,0)



