module XContest.Parse
  ( xContestDay
  ) where

import Control.Lens (Prism', prism)
import Data.Text    (Text)
import Data.Time    (Day, defaultTimeLocale, formatTime, parseTimeM)

import qualified Data.Text as T

xContestDay :: Prism' Text Day
xContestDay =
  let dateFmt = "%d.%m.%y"
      locale = defaultTimeLocale -- TODO: AEST?
  in prism
       (T.pack . formatTime locale dateFmt)
       (parseTimeM True locale dateFmt . T.unpack)
