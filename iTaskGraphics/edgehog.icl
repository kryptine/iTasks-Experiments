implementation module edgehog

import iTasks

pi :== 3.1415926

shared_edges :: Shared Edgehog
shared_edges = sharedStore "Edgehog" {nrOfEdges = 10
                                     ,edgeLength= 50
                                     ,rBound    = {low=100,high=255}
                                     ,gBound    = {low=  0,high=128}
                                     ,bBound    = {low= 30,high= 50}
                                     }

:: Edgehog = { nrOfEdges :: !Int
             , edgeLength:: !Int
             , rBound    :: !CBound
             , gBound    :: !CBound
             , bBound    :: !CBound
             }
:: CBound  = { low       :: !Int
             , high      :: !Int
             }

derive class iTask Edgehog, CBound

edgehog :: Task Void
edgehog	= (    viewSharedInformation   (Title "Edgehog") [ViewWith hog] shared_edges 
          -&&- updateSharedInformation (Title "Edit") [] shared_edges
          ) @ (const Void)
where
	hog {nrOfEdges,edgeLength,rBound,gBound,bBound}
		= SvgTag [WidthAttr "100mm",HeightAttr "100mm"] [VersionAttr "1.1"] 
	             [RectElt [WidthAttr "100mm",HeightAttr "100mm"] 
	                      [FillAttr   (PaintColor (SVGColorText "white") Nothing)
	                      ,StrokeAttr (PaintColor (SVGColorText "black") Nothing)
	                      ]
	             :[LineElt [] [ X1Attr ("50",MM), Y1Attr ("50",MM)
	                          , X2Attr (toString (50.0 + (toReal edgeLength)*(cos a)),MM), Y2Attr (toString (50.0 + (toReal edgeLength)*(sin a)),MM)
	                          , StrokeWidthAttr (StrokeWidthLength (toString ((a+0.5)/(2.0*pi)),MM))
	                          , StrokeAttr      (PaintColor (SVGRGB (gradient a rBound)
	                                                                (gradient a gBound) 
	                                                                (gradient a bBound)
	                                                        ) Nothing)
	                          ]
	              \\ a <- [0.0, (2.0 / (toReal nrOfEdges))*pi .. 2.0*pi]
	              ]
	             ]
	gradient a {low,high}
		= min 255 (max 0 (low` + toInt ((toReal (high`-low`))*a/(2.0*pi))))
	where
		low`	= min low high
		high`	= max low high
