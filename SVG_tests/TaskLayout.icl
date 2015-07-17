implementation module TaskLayout

import iTasks
from Graphics.Scalable import :: GridDimension(..), :: GridLayout(..), :: GridMajor(..), :: GridXLayout(..), :: GridYLayout(..), :: ImageAlign(..), :: XAlign(..), :: YAlign(..)

:: TaskUI         = { components  :: Map TaskTag (Maybe UIClass,Maybe TaskLayout) 
                    , taskLayout  :: TaskLayout
                    }
:: UIClass        = UIClass !String
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

derive class iTask TaskTag, TaskLayout, XAlign, YAlign, GridDimension, GridMajor, GridXLayout, GridYLayout, HostProperties, Dim

instance tune ArrangeTaskUI where tune _ t = t

:: ArrangeTaskUI = ArrangeTaskUI (TaskUI -> TaskLayout)

instance tune UIClass where tune _ t = t

instance fromString UIClass where fromString x = UIClass x

withLayout :: TaskLayout -> TaskTag
withLayout layout = LayoutTag layout

:: TaskTag = TaskTag !Int | SystemTag !TaskId | LayoutTag !TaskLayout

internal_tag_source :: Shared Int
internal_tag_source = sharedStore "internal_tag_source" 0

getTag :: Task TaskTag
getTag = get internal_tag_source >>= \nr ->
         set (nr+1) internal_tag_source >>|
         return (TaskTag nr)

getTags :: Int -> Task [TaskTag]
getTags n
| n <= 0	= return []
| otherwise = get internal_tag_source >>= \nr ->
              set (nr+n) internal_tag_source >>|
              return (map TaskTag [nr .. nr+n-1])

tagTask :: TaskTag (Task a) -> Task a
tagTask _ t = t
