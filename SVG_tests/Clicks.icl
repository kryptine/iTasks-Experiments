module Clicks

import StdDebug
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import qualified Data.Set as DS
import Graphics.Scalable.Internal

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> clicks)
                          ] world


clicks :: Task String
clicks = updateInformation "Click example" [imageUpdate id mkImg (\_ _ -> Nothing) (const id)] "(default state)"

mkImg :: String *TagSource -> Image String
mkImg str _
  #! r = rect (px 100.0) (px 100.0)
  #! t = text (normalFontDef "Arial" 10.0) str <@< { fill = toSVGColor "white" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [t] (Just r) <@< { onclick = \n _ -> case n of
                                                                                      1 -> "one click"
                                                                                      2 -> "double click"
                                                                                      n -> toString n +++ " clicks"
                                                                , local = False }
