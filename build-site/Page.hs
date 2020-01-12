{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Page
  ( Page(..)
  , pageTitle
  , pageContent
  , pageUrl
  , pageTeaser
  , pageImage
  , pageAuthor
  , pagePublished
  , pageEventStart
  , pageEventFinish
  , sortPagesByDate
  , comparingPagesByDate
  , sortPages
  , comparingPages
  , pageDaysDistant
  , daysDistant
  , pathToRootPath
  , pageFilePath
  , relativeUrl
  , pageRelativeUrl
  , pageRelativizeUrl
  , isActivePage
  ) where

import Control.Lens                 (makeLenses)
import Control.Lens.Operators
import Data.Aeson                   (FromJSON, ToJSON)
import Data.Aeson.Generic.Shorthand (CamelFields, GenericToFromJSON (..))
import Data.Binary.Instances.Time   ()
import Data.List                    (sortBy)
import Data.Maybe                   (catMaybes)
import Data.Ord                     (comparing)
import Data.Text                    (Text)
import Data.Text.Lens               (unpacked)
import Data.Time                    (Day, diffDays)
import Development.Shake.Classes    (Binary)
import Development.Shake.FilePath   ((</>))
import GHC.Generics                 (Generic)
import System.FilePath              (joinPath, splitDirectories)

import qualified Data.Text as T

-- | Page is the data structure that we load from MarkDown files.
-- It has a lot of `Maybe` fields, because most metadata is optional.
-- We also keep it around in other data structures (for now).
data Page = Page
  { _pageTitle       :: Text
  , _pageContent     :: Text
  , _pageUrl         :: Text -- E.g. "info/faq", with the filepath for that url being "info/faq/index.html"
  , _pageTeaser      :: Maybe Text
  , _pageImage       :: Maybe Text
  , _pageAuthor      :: Maybe Text
  , _pagePublished   :: Maybe Day
  , _pageEventStart  :: Maybe Day
  , _pageEventFinish :: Maybe Day
  } deriving (Generic, Eq, Ord, Show, Binary)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[CamelFields] Page)
$(makeLenses ''Page)

sortPagesByDate :: Day -> [Page] -> [Page]
sortPagesByDate today = sortBy (comparingPagesByDate today)

comparingPagesByDate :: Day -> Page -> Page -> Ordering
comparingPagesByDate today =
  comparing (pageDaysDistant today)

sortPages :: Day -> [Page] -> [Page]
sortPages today = sortBy (comparingPages today)

comparingPages :: Day -> Page -> Page -> Ordering
comparingPages today l r =
  case comparingPagesByDate today l r of
    EQ -> compare l r -- Fall back on Ord instance
    x -> x

pageDaysDistant :: Day -> Page -> Maybe Integer
pageDaysDistant today Page{..} =
  let distances = daysDistant today  <$> catMaybes [_pagePublished, _pageEventStart, _pageEventFinish]
  in case distances of
    [] -> Nothing
    _  -> Just $ minimum distances

daysDistant :: Day -> Day -> Integer
daysDistant l r = abs $ diffDays l r

pathToRootPath :: FilePath -> FilePath
pathToRootPath p =
  let depth = length (splitDirectories p) - 1
  in joinPath . take depth $ repeat "../"

pageFilePath :: Page -> FilePath
pageFilePath page =
  -- We change /foo/bar.html into /foo/bar/index.html to give it a clean path (when a server serves the index automatically)
  page ^. pageUrl . unpacked </> "index.html"

relativeUrl :: Page -> FilePath -> FilePath
relativeUrl from to =
  pathToRootPath (pageFilePath from) </> to

pageRelativeUrl :: Page -> Page -> FilePath
pageRelativeUrl from to =
  relativeUrl from (to ^. pageUrl . unpacked)

pageRelativizeUrl :: Page -> Page -> Page
pageRelativizeUrl from to =
  to & pageUrl . unpacked %~ relativeUrl from

isActivePage :: Page -> Page -> Bool
isActivePage activePage page =
  (page ^. pageUrl) `T.isPrefixOf` (activePage ^. pageUrl)

