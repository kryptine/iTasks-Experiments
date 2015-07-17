implementation module Derived

import StdEnv, Data.Maybe, PView
from Data.List import splitWith

singletonLens :: Lens [a] [a] a a
singletonLens = {lget = \[x:_] -> x, lput = \_ x -> [x]}

readOnlyLens :: Lens a a a Void
readOnlyLens = {lget = id, lput = const}

joinLists :: (PView p [a] [a] MyWorld) (PView p [a] [a] MyWorld) (a -> Bool) -> (PView p [a] [a] MyWorld) | TC a & TC p
joinLists l1 l2 f = applyLens (applyTranslation (join l1 l2) trans) {lget=lget,lput=lput}
where
    trans p = (p,p)
    lget (l1,l2) = l1 ++ l2
    lput _ l  = splitWith f l

maybeParam :: a (PView p a a MyWorld) -> (PView (Maybe p) a a MyWorld) | TC p
maybeParam def view = applyTranslation (union (constantView def) view notify notify) trans
where
    trans Nothing   = Left Void
    trans (Just p)  = Right p
    notify _ _ _    = const False

listFilterSplit :: (p a -> Bool) -> Split p [a] [a] 
listFilterSplit filterFun = {sget=sget,sput=sput}
where
    sget par is = filter (filterFun par) is
    sput par is ws
        = let (ds,us) = splitWith (filterFun par) is
          in (us ++ ws, notifyFun (ds ++ ws))

    notifyFun ws par = any (filterFun par) ws
   
constantView :: a -> (PView Void a a MyWorld)
constantView def = Source {Source | get = get` def, put = put`}
where
    get` def Void myworld = (Ok def,myworld)
    put` Void _ myworld = (Ok (const True),myworld)