definition module Derived

import StdEnv, Data.Maybe, PView

singletonLens :: Lens [a] [a] a a

readOnlyLens :: Lens a a a Void

joinLists :: (PView p [a] [a] MyWorld) (PView p [a] [a] MyWorld) (a -> Bool) -> (PView p [a] [a] MyWorld) | TC a & TC p

maybeParam :: a (PView p a a MyWorld) -> (PView (Maybe p) a a MyWorld) | TC p

listFilterSplit :: (p a -> Bool) -> Split p [a] [a] 
    
constantView :: a -> (PView Void a a MyWorld)