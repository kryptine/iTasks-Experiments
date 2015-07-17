definition module TaskLayout

import iTasks
from Graphics.Scalable import :: GridDimension(..), :: GridLayout(..), :: GridMajor(..), :: GridXLayout(..), :: GridYLayout(..), :: ImageAlign(..), :: XAlign(..), :: YAlign(..)

:: TaskUI         = { components  :: Map TaskTag (Maybe UIClass,Maybe TaskLayout) 
                    , taskLayout  :: TaskLayout
                    }
:: UIClass
:: TaskLayout     = Collage [(Dim,Dim)] [TaskTag]
                  | Above   [XAlign]    [TaskTag]
                  | Beside  [YAlign]    [TaskTag]
                  | Grid    GridDimension GridLayout [ImageAlign] [TaskTag]
                  | Host    HostProperties TaskTag [TaskTag]
:: HostProperties = { width      :: Dim
                    , height     :: Dim
                    , scrollable :: Bool
                    }
:: Dim            = Pixel Real | Percentage Real | Auto

instance tune ArrangeTaskUI

:: ArrangeTaskUI = ArrangeTaskUI (TaskUI -> TaskLayout)

instance tune       UIClass
instance fromString UIClass

withLayout :: TaskLayout -> TaskTag

:: TaskTag = TaskTag !Int | SystemTag !TaskId | LayoutTag !TaskLayout
derive class iTask TaskTag

getTag  :: Task TaskTag
getTags :: Int -> Task [TaskTag]
tagTask :: TaskTag (Task a) -> Task a
