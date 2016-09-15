definition module EdTab

import syncol

:: SyntaxColours =
	{ textColour		:: !Colour
	, backgroundColour	:: !Colour
	, marginColour		:: !Colour
	, tabColour			:: !Colour
	, commentColour		:: !Colour
	, stringColour		:: !Colour
	, charColour		:: !Colour
	, keywordColour		:: !Colour
	, typedefColour		:: !Colour
	, typedeclColour	:: !Colour
	}
	
::	Colour
	=	RGB RGBColour
	|	Black		| White
	|	DarkGrey	| Grey		| LightGrey	// 75%, 50%, and 25% Black
	|	Red			| Green		| Blue
	|	Cyan		| Magenta	| Yellow
	
::	RGBColour
	=	{	r	:: !Int						// The contribution of red
		,	g	:: !Int						// The contribution of green
		,	b	:: !Int						// The contribution of blue
		}	

DefaultSyntaxColours :: SyntaxColours

:: *Picture

newPicture :: *Picture

asString :: *Picture -> (!String, !Picture)

tabDrawStringC :: !(!Info,!String) !SyntaxColours !*Picture -> *Picture

